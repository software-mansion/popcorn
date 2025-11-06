defmodule Popcorn.Support.AtomVM do
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

  require Logger

  @unix_path "test/fixtures/unix/AtomVM"

  defguardp is_eval_type(type) when type in [:erlang_module, :erlang_expr, :elixir]

  defmacro assert_result(result, expected) do
    quote do
      assert unquote(expected) = unquote(result)
    end
  end

  def assert_is_module(eval_result) do
    is_elixir_module = match?({:module, _name, _bin, _defmodule_eval_res}, eval_result)
    is_erlang_module = match?({:module, _name}, eval_result)

    assert is_elixir_module or is_erlang_module,
           "Returned value isn't a module: #{inspect(eval_result)}"
  end

  def test_target(target) do
    Application.put_env(:popcorn, :test_target, target)
  end

  def test_target() do
    Application.fetch_env!(:popcorn, :test_target)
  end

  @doc """
  Evaluates a string using precompiled evaluation code.
  """
  def eval(code, type, opts \\ []) when is_binary(code) and is_eval_type(type) do
    failing = Keyword.get(opts, :failing, false)
    info = try_eval(code, type, opts)

    if failing do
      assert info.exit_status != 0,
             "Expected code evaluation to fail, check '#{info.log_path}' for more information"
    else
      assert info.exit_status == 0,
             "Code evaluation failed, check '#{info.log_path}' for more information"
    end

    info.result
  end

  @doc """
  Evaluates a string using precompiled evaluation code.

  Doesn't crash in case of failure, always returns an `info` map.
  """
  def try_eval(code, type, opts \\ []) when is_binary(code) and is_eval_type(type) do
    run_dir = Keyword.fetch!(opts, :run_dir)
    fragment = type |> to_ast_fragment_type() |> ast_fragment()
    bundle_path = bundle_path(fragment)

    if not File.exists?(bundle_path) do
      raise "Compile eval module before using it"
    end

    try_run(bundle_path, run_dir, code: code)
  end

  @doc """
  Runs compiled .avm bundle with passed args.
  """
  def run(bundle_path, run_dir, args \\ []) do
    assert %{exit_status: 0, result: result} = try_run(bundle_path, run_dir, args)
    result
  end

  def try_run(bundle_path, run_dir, args \\ []) do
    info = do_try_run(test_target(), bundle_path, run_dir, Map.new(args))

    Logger.info("""
    Evaluating code on AtomVM finished
    exit status: #{inspect(info.exit_status)}
    log path: "#{info.log_path}"
    result: #{inspect(info.result, pretty: true)}
    output: #{if String.trim(info.output) == "", do: "no output generated", else: info.output}\
    """)

    info
  end

  defp do_try_run(:unix, bundle_path, run_dir, args) do
    result_path = Path.join(run_dir, "result.bin")
    args_path = Path.join(run_dir, "args.bin")
    log_path = Path.join(run_dir, "logs.txt")
    out_path = Path.join(run_dir, "out.txt")

    args |> :erlang.term_to_binary() |> then(&File.write(args_path, &1))

    if System.shell("which '#{@unix_path}'") |> elem(1) != 0 do
      raise """
      AtomVM not found, please run `mix popcorn.build_runtime` \
      or put `config :popcorn, runtime_path: "path/to/AtomVM"` in your config.exs
      """
    end

    cmd =
      if System.get_env("CI") == "true" do
        "AVM_RUN_DIR='#{run_dir}' '#{@unix_path}' '#{bundle_path}'"
      else
        # $() suppresses sh error about process signal traps, i.e. when AVM crashes
        ~s|$(AVM_RUN_DIR='#{run_dir}' '#{@unix_path}' '#{bundle_path}' 2>'#{log_path}' 1>'#{out_path}')|
      end

    File.write!(log_path, "Run command: #{cmd}\n\n\n")
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

  defp do_try_run(:wasm, bundle_path, run_dir, args) do
    args = args |> :erlang.term_to_binary() |> Base.encode16()

    log_path = Path.join(run_dir, "logs.txt")

    log_task =
      Task.async(fn ->
        Stream.repeatedly(fn -> receive do: (log -> log) end)
        |> Stream.take_while(&(&1 != :eof))
        |> Stream.into(File.stream!(log_path))
        |> Stream.run()
      end)

    page = Popcorn.Support.Browser.new_page(bundle_path, &send(log_task.pid, &1))

    snippet = """
    try {
      const result = await popcorn.call("#{args}", {});
      return result;
    } catch (e) {
      return {error: true}
    }
    """

    result = Playwright.Page.evaluate(page, "async () => {#{snippet}}")

    send(log_task.pid, :eof)
    Task.await(log_task)

    output =
      File.read!(log_path)
      # Only include lines prefixed with [popcorn stdout]
      |> then(&Regex.replace(~r/^(?!\[popcorn stdout\]).*\n?/m, &1, "", global: true))
      # And remove the prefix from them
      |> then(&Regex.replace(~r/^\[popcorn stdout\] /m, &1, "", global: true))
      # FIXME: Filter out logs that pollute stdout
      |> then(
        &Regex.replace(~r/^Downloading .*.beam failed, HTTP failure.*\n/m, &1, "", global: true)
      )
      |> then(&Regex.replace(~r/^Return value:.*\n/m, &1, "", global: true))

    case result do
      %{data: data} ->
        result = data |> Base.decode16!() |> :erlang.binary_to_term()
        %{exit_status: 0, result: result}

      %{error: true} ->
        %{exit_status: 1, result: nil}
    end
    |> Map.merge(%{log_path: log_path, output: output})
  end

  defp bundle_path(ast) do
    hash = ast |> :erlang.phash2() |> to_string()
    build_dir = Path.join(compile_dir(), hash)
    Path.join(build_dir, "bundle.avm")
  end

  @doc """
  Appends passed ast to common code that reads input from args.bin and writing to result.bin.
  Ast may reference `args` variable that is read from input file while calling `run/3`.
  """
  def compile_quoted(ast) do
    bundle_path = bundle_path(ast)
    build_dir = Path.dirname(bundle_path)
    stale = not File.exists?(bundle_path)

    if stale do
      File.rm_rf!(build_dir)
      File.mkdir_p!(build_dir)

      ast
      |> module(test_target())
      |> run_elixirc(build_dir)
    end

    beam_paths = Path.wildcard(Path.join(build_dir, "*.beam"))

    Popcorn.bundle(
      extra_beams: beam_paths,
      start_module: RunExpr,
      out_dir: build_dir
    )

    bundle_path
  end

  defp compile_dir() do
    Path.join([File.cwd!(), "tmp/modules", to_string(test_target())])
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

  defp module(code, :wasm) do
    quote location: :keep do
      defmodule RunExpr do
        @moduledoc false
        alias Popcorn.Wasm

        @compile autoload: false, no_warn_undefined: [:atomvm, Wasm]

        def start() do
          Process.register(self(), :main)
          Wasm.register("main")

          receive do
            wasm_msg -> Wasm.handle_message!(wasm_msg, &handle_wasm_msg/1)
          end

          :ok
        end

        defp handle_wasm_msg({:wasm_call, args}) do
          args = args |> Base.decode16!() |> :erlang.binary_to_term()
          result = run(args)
          result = result |> :erlang.term_to_binary() |> Base.encode16()
          {:resolve, result, :ok}
        end

        defp run(args) do
          _supppress_unused = args
          unquote(code)
        end
      end
    end
  end

  defp module(code, :unix) do
    quote do
      defmodule RunExpr do
        @moduledoc false
        @compile autoload: false, no_warn_undefined: :atomvm

        def start() do
          run_dir = :os.getenv(~c"AVM_RUN_DIR")
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
