defmodule RunInAtomVM do
  import ExUnit.Assertions

  @atomvm_path Application.compile_env!(:fission_lib, :atomvm_path)

  def expr(code, tmp_dir, opts \\ []) do
    compile(code, tmp_dir, Keyword.keys(opts))
    run(tmp_dir, tmp_dir, opts)
  end

  def compile(code, dir, vars) do
    build_path = "#{dir}/_build"
    File.rm_rf!(build_path)
    File.mkdir_p!(build_path)

    [{mod, beam}] = code |> module(vars, build_path) |> Code.compile_quoted()
    beam_name = "#{build_path}/#{inspect(mod)}.beam"
    File.write!(beam_name, beam)

    FissionLib.pack(
      artifacts: [beam_name],
      start_module: RunExpr,
      output_path: "#{build_path}/bundle.avm"
    )

    :ok
  end

  def run(dir, tmp_dir, opts \\ []) do
    build_path = "#{dir}/_build"
    opts = opts |> Map.new() |> :erlang.term_to_binary()
    File.write!("#{build_path}/opts.bin", opts)

    {_output, ret_val} =
      System.shell("#{@atomvm_path} #{build_path}/bundle.avm &> #{tmp_dir}/log.txt")

    assert ret_val == 0,
           "Executing code in AtomVM failed, see #{Path.relative_to_cwd(tmp_dir)}/log.txt for logs"

    File.read!("#{build_path}/result.bin") |> :erlang.binary_to_term()
  end

  defp module(code, vars, build_path) do
    assignments =
      for v <- vars do
        quote do
          unquote(Macro.var(v, nil)) = Map.fetch!(opts, unquote(v))
        end
      end

    quote do
      defmodule RunExpr do
        @compile autoload: false, no_warn_undefined: :atomvm

        def start() do
          opts = read_opts()
          result = run(opts)
          write_result(result)
          :ok
        end

        defp run(opts) do
          unquote_splicing(assignments)
          unquote(code)
        end

        defp read_opts() do
          {:ok, fd} = :atomvm.posix_open(~c"#{unquote(build_path)}/opts.bin", [:o_rdonly])
          {:ok, opts} = :atomvm.posix_read(fd, 1_000_000)
          :erlang.binary_to_term(opts)
        end

        defp write_result(result) do
          result_bin = :erlang.term_to_binary(result)

          {:ok, fd} =
            :atomvm.posix_open(
              ~c"#{unquote(build_path)}/result.bin",
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
