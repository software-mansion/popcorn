defmodule FissionLib.Support.AtomVM do
  @moduledoc """
  Used to run AtomVM instances in tests.

  Provides convenience assertions and `eval/3` function
  to evaluate Erlang or Elixir code as a string.

  `compile_quoted/2` and `run/3` are lower level API used for tests
  that run one-off code without evaluation. This works by first compiling
  a module with some AST that can use `var!` for runtime input.

  This input is provided by serializing terms to `opts.bin` which is read
  when this module runs `start/0`. We then write code output to `result.bin`.

  Compiled files are cached with phash2 key of AST contents under tmp/modules/ directory.
  All eval tests run the same underlying code so `eval` expects
  module to be compiled and cached prior to call.
  Input and output files for tests running concurently are keyed by `AVM_RUN_DIR` which stores path prefix.
  """
  import ExUnit.Assertions

  @atomvm_path Application.compile_env!(:fission_lib, :atomvm_path)
  # mix always compiles files from project root
  @compile_dir Path.join([File.cwd!(), "tmp", "modules"])

  defguardp is_ast(ast) when tuple_size(ast) == 3
  defguardp is_eval_type(type) when type in [:erlang_module, :erlang_expr, :elixir]

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
    run_dir = Keyword.fetch!(opts, :run_dir)
    failing = Keyword.get(opts, :failing, false)
    fragment = type |> to_ast_fragment_type() |> ast_fragment()

    if not compiled?(fragment) do
      raise "Compile eval module before using it"
    end

    info =
      fragment
      |> compile_quoted()
      |> run(run_dir, code: code)

    if failing do
      assert info.exit_status != 0,
             "Expected code evaluation to fail, check '#{info.log_path}' for more information"
    else
      assert info.exit_status == 0,
             "Code evaluation failed, check '#{info.log_path}' for more information"
    end

    info.result
  end

  def run(bundle_path, run_dir, args \\ []) do
    result_path = Path.join(run_dir, "result.bin")
    args_path = Path.join(run_dir, "args.bin")
    log_path = Path.join(run_dir, "logs.txt")

    args
    |> Map.new()
    |> :erlang.term_to_binary()
    |> then(&File.write(args_path, &1))

    # $() suppresses sh error about process signal traps, i.e. when AVM crashes
    cmd = "$(AVM_RUN_DIR='#{run_dir}' #{@atomvm_path} #{bundle_path} >>#{log_path} 2>&1)"
    File.write!(log_path, "Run command: #{cmd}\n")
    {output, exit_status} = System.shell(cmd)

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

  def compile_quoted(ast) when is_ast(ast) do
    hash = ast |> :erlang.phash2() |> to_string()
    build_dir = Path.join(@compile_dir, hash)
    avm_path = Path.join(build_dir, "bundle.avm")

    stale = not File.exists?(avm_path)

    if stale do
      File.rm_rf!(build_dir)
      File.mkdir_p!(build_dir)

      [beam_path] =
        ast
        |> module()
        |> run_elixirc(build_dir)

      FissionLib.pack(artifacts: [beam_path], start_module: RunExpr, out_path: avm_path)
    end

    avm_path
  end

  defp run_elixirc(ast, dir) do
    copy_artifacts_to_dir = fn path ->
      file_name = Path.basename(path)
      destination_path = Path.join(dir, file_name)
      File.cp!(path, destination_path)

      destination_path
    end

    build_dir = Path.join(dir, "_build")
    File.rm_rf!(build_dir)
    File.mkdir_p!(build_dir)

    source_path = Path.join(build_dir, "code.ex")
    File.write!(source_path, Macro.to_string(ast))
    {_output, 0} = System.shell("elixirc #{source_path} -o #{build_dir}")

    files = build_dir |> Path.join("*.{ex,beam}") |> Path.wildcard()
    paths = Enum.map(files, copy_artifacts_to_dir)
    File.rm_rf!(build_dir)

    Enum.filter(paths, &(Path.extname(&1) == ".beam"))
  end

  def ast_fragment(:eval_elixir) do
    quote do
      :elixir.start([], [])

      args.code
      |> Code.eval_string([], __ENV__)
      |> elem(0)
    end
  end

  def ast_fragment(:eval_erlang_expr) do
    quote do
      code = args.code |> :erlang.binary_to_list()
      {:ok, tokens, _} = :erl_scan.string(code)
      IO.puts("Scanned")
      {:ok, exprs} = :erl_parse.parse_exprs(tokens)
      IO.puts("Parsed")
      {:value, value, _new_bindings} = :erl_eval.exprs(exprs, [])
      value
    end
  end

  def ast_fragment(:eval_erlang_module) do
    quote do
      code = args.code |> :erlang.binary_to_list()

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

  defp to_ast_fragment_type(:elixir), do: :eval_elixir
  defp to_ast_fragment_type(:erlang_module), do: :eval_erlang_module
  defp to_ast_fragment_type(:erlang_expr), do: :eval_erlang_expr

  defp module(code) do
    quote do
      defmodule RunExpr do
        @compile autoload: false, no_warn_undefined: :atomvm

        def start() do
          run_dir = :atomvm.posix_getenv(~c"AVM_RUN_DIR")
          args = read_args(run_dir)
          result = run(args)
          write_result(result, run_dir)
          :ok
        end

        defp run(args) do
          _supppress_unused = args
          unquote(code)
        end

        defp read_args(run_dir) do
          path = ~c"#{run_dir}/args.bin"
          {:ok, fd} = :atomvm.posix_open(path, [:o_rdonly])
          {:ok, opts} = :atomvm.posix_read(fd, 1_000_000)
          :erlang.binary_to_term(opts)
        end

        defp write_result(result, run_dir) do
          result_bin = :erlang.term_to_binary(result)
          path = ~c"#{run_dir}/result.bin"
          {:ok, fd} = :atomvm.posix_open(path, [:o_creat, :o_wronly], 0o644)
          {:ok, _size} = :atomvm.posix_write(fd, result_bin)
          :ok
        end
      end
    end
  end
end
