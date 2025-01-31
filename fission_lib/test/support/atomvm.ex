defmodule FissionLib.AtomVM do
  @moduledoc """
  Used to run AtomVM instances in tests.

  Provides convenience assertions and `eval/3` function
  to evaluate Erlang or Elixir code as a string.

  `compile_quoted/2` and `run_with_bindings/2` are lower level API used for tests
  that run one-off code without evaluation. This works by first compiling
  a module with some AST that can use `var!` for runtime input.

  This input is provided by serializing terms to `opts.bin` which is read
  when this module runs `start/0`. We then write code output to `result.bin`.

  Compiled files are cached with phash2 key of AST contents under app's priv directory.
  All eval tests run the same underlying code so `eval` expects
  module to be compiled and cached prior to call.
  """
  import ExUnit.Assertions

  @atomvm_path Application.compile_env!(:fission_lib, :atomvm_path)
  @compile_dir :code.priv_dir(:fission_lib)

  defguardp is_ast(ast) when tuple_size(ast) == 3
  defguardp is_eval_type(type) when type in [:erlang, :erlang_expr, :elixir]

  defmacro assert_result(result, expected) do
    quote do
      assert unquote(expected) = unquote(result)
    end
  end

  def assert_is_module(eval_result) do
    is_elixir_module = match?({:module, _name, _bin, _defmodule_eval_res}, eval_result)
    is_erlang_module = match?({:module, _name}, eval_result)

    assert is_elixir_module or is_erlang_module, "Returned value isn't a module"
  end

  def eval(code, type, opts \\ []) when is_binary(code) and is_eval_type(type) do
    failing = Keyword.get(opts, :failing, false)
    fragment = eval_fragment(type)

    if not compiled?(fragment) do
      raise "Compile eval module before using it"
    end

    info =
      fragment
      |> compile_quoted([:code])
      |> run_with_bindings(code: code)

    if failing do
      assert info.exit_status != 0,
             "Expected code evaluation to fail, check '#{info.log_path}' for more information"
    else
      assert info.exit_status == 0,
             "Code evaluation failed, check '#{info.log_path}' for more information"
    end

    info.result
  end

  def run_with_bindings(bundle_path, bindings) do
    dir = Path.dirname(bundle_path)
    run_id = :erlang.unique_integer([:positive])

    result_path = Path.join(dir, "result-#{run_id}.bin")
    bindings_path = Path.join(dir, "opts-#{run_id}.bin")

    logs_dir = Path.join(dir, "logs")
    File.mkdir_p!(logs_dir)

    unix_now = DateTime.now!("Etc/UTC") |> DateTime.to_iso8601()
    log_path = Path.join(logs_dir, "#{unix_now}.txt")

    bindings
    |> Map.new()
    |> :erlang.term_to_binary()
    |> then(&File.write(bindings_path, &1))

    # $() suppresses sh error about process signal traps, i.e. when AVM crashes
    {output, exit_status} =
      System.shell("$(AVM_RUN_ID=#{run_id} #{@atomvm_path} #{bundle_path} &> #{log_path})")

    result =
      case File.read(result_path) do
        {:ok, result} -> :erlang.binary_to_term(result)
        {:error, _reason} -> nil
      end

    %{
      exit_status: exit_status,
      output: output,
      result: result,
      log_path: log_path
    }
  end

  def compiled?(ast) do
    hash = ast |> :erlang.phash2() |> to_string()
    build_dir = Path.join(@compile_dir, hash)

    File.exists?(build_dir)
  end

  def delete_run_artifacts do
    logs =
      @compile_dir
      |> Path.join("**/logs")
      |> Path.wildcard()

    opts =
      @compile_dir
      |> Path.join("**/opts-*.bin")
      |> Path.wildcard()

    results =
      @compile_dir
      |> Path.join("**/result-*.bin")
      |> Path.wildcard()

    Enum.each(logs ++ opts ++ results, &File.rm_rf!/1)
  end

  def compile_quoted(ast, bindings) when is_ast(ast) do
    hash = ast |> :erlang.phash2() |> to_string()
    build_dir = Path.join(@compile_dir, hash)
    avm_path = Path.join(build_dir, "bundle.avm")

    stale = not File.exists?(avm_path)

    if stale do
      File.rm_rf!(build_dir)
      File.mkdir_p!(build_dir)

      [beam_path] =
        ast
        |> module(bindings, build_dir)
        |> run_elixirc(build_dir)

      FissionLib.pack(artifacts: [beam_path], start_module: RunExpr, out_path: avm_path)
    end

    avm_path
  end

  defp run_elixirc(ast, dir) do
    copy_beam_to_dir = fn path ->
      beam_name = Path.basename(path)
      destination_path = Path.join(dir, beam_name)
      File.cp!(path, destination_path)

      destination_path
    end

    build_dir = Path.join(dir, "_build")
    File.rm_rf!(build_dir)
    File.mkdir_p!(build_dir)

    source_path = Path.join(build_dir, "code.ex")
    File.write!(source_path, Macro.to_string(ast))
    {_output, 0} = System.shell("elixirc #{source_path} -o #{build_dir}")

    beams = build_dir |> Path.join("*.beam") |> Path.wildcard()
    paths = Enum.map(beams, copy_beam_to_dir)
    File.rm_rf!(build_dir)

    paths
  end

  def eval_fragment(:elixir) do
    quote do
      :elixir.start([], [])

      var!(code)
      |> Code.eval_string([], __ENV__)
      |> elem(0)
    end
  end

  def eval_fragment(:erlang_expr) do
    quote do
      code = var!(code) |> :erlang.binary_to_list()
      {:ok, tokens, _} = :erl_scan.string(code)
      IO.puts("Scanned")
      {:ok, exprs} = :erl_parse.parse_exprs(tokens)
      IO.puts("Parsed")
      {:value, value, _new_bindings} = :erl_eval.exprs(exprs, [])
      value
    end
  end

  def eval_fragment(:erlang) do
    quote do
      code = var!(code) |> :erlang.binary_to_list()

      parse_form = fn form_tok ->
        {:ok, form} = :erl_parse.parse_form(form_tok)
        form
      end

      split_on_dots = fn
        {:dot, _} = f, current -> {:cont, Enum.reverse([f | current]), []}
        f, current -> {:cont, [f | current]}
      end

      ensure_empty_acc = fn [] -> {:cont, []} end

      {:ok, tokens, _} = :erl_scan.string(code)

      compiler_opts = [
        :deterministic,
        :return_errors,
        :compressed,
        :no_spawn_compiler_process,
        :no_docs
      ]

      {:ok, module, module_bin} =
        Enum.chunk_while(tokens, [], split_on_dots, ensure_empty_acc)
        |> Enum.map(parse_form)
        |> :compile.noenv_forms(compiler_opts)

      :code.load_binary(module, ~c"nofile", module_bin)
    end
  end

  defp module(code, bindings, build_path) do
    assignments =
      for v <- bindings do
        quote do
          unquote(Macro.var(v, nil)) = Map.fetch!(opts, unquote(v))
        end
      end

    quote do
      defmodule RunExpr do
        @compile autoload: false, no_warn_undefined: :atomvm

        def start() do
          run_id = :atomvm.posix_getenv(~c"AVM_RUN_ID")
          opts = read_opts(run_id)
          result = run(opts)
          write_result(result, run_id)
          :ok
        end

        defp run(opts) do
          _no_warn_unused = opts
          unquote_splicing(assignments)
          unquote(code)
        end

        defp read_opts(run_id) do
          {:ok, fd} =
            :atomvm.posix_open(~c"#{unquote(build_path)}/opts-#{run_id}.bin", [:o_rdonly])

          {:ok, opts} = :atomvm.posix_read(fd, 1_000_000)
          :erlang.binary_to_term(opts)
        end

        defp write_result(result, run_id) do
          result_bin = :erlang.term_to_binary(result)

          {:ok, fd} =
            :atomvm.posix_open(
              ~c"#{unquote(build_path)}/result-#{run_id}.bin",
              [:o_creat, :o_wronly],
              String.to_integer("644", 8)
            )

          {:ok, _size} = :atomvm.posix_write(fd, result_bin)
          :ok
        end
      end
    end
  end
end
