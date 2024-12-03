defmodule FissionLib.Build do
  @moduledoc false
  alias FissionLib.CoreErlangUtils

  @doc """
  Runs `compile/2` and `patch/3`, then packs output beams
  into a single *.avm file.
  """
  def build(out_dir, stdlib_beam_paths, add_tracing) do
    build_dir = Path.join(out_dir, "fission_lib")
    File.rm_rf!(build_dir)
    File.mkdir_p!("#{build_dir}/patches_ebin")
    File.mkdir_p!("#{build_dir}/final_ebin")

    compile("patches", "#{build_dir}/patches_ebin")

    patch_beam_paths = Path.wildcard("#{build_dir}/patches_ebin/*.beam")

    patch(stdlib_beam_paths, patch_beam_paths, "#{build_dir}/final_ebin",
      add_tracing: add_tracing
    )

    :packbeam_api.create(
      ~c"#{out_dir}/fission_lib.avm",
      Path.wildcard("#{build_dir}/final_ebin/*.beam") |> Enum.map(&String.to_charlist/1)
    )

    :ok
  end

  @doc """
  Compiles the parts of Erlang and Elixir standard libraries
  that are customized for the AtomVM, as well as some AtomVM-specific
  utilities.

  The sources to be compiled reside in the `patches` directory.
  """
  def compile(patches_dir, out_dir) do
    yrl_dir = Path.join(out_dir, "yrl")
    File.mkdir_p!(yrl_dir)

    ex_paths = Path.wildcard("#{patches_dir}/**/*.ex")

    # Compiling each file separately, like AtomVM does it.
    # Compiling together may break something, as these modules
    # will override stdlib modules.
    process_async(ex_paths, fn file ->
      IO.puts("Compiling #{file}")
      {_out, 0} = System.shell("elixirc --ignore-module-conflict -o #{out_dir} #{file}")
    end)

    yrl_paths = Path.wildcard("#{patches_dir}/**/*.yrl")

    process_async(yrl_paths, fn file ->
      IO.puts("Compiling #{file}")
      {_out, 0} = System.shell("erlc -o #{yrl_dir} #{file}")
    end)

    erl_paths = Path.wildcard("#{patches_dir}/**/*.{erl,S}") ++ Path.wildcard("#{yrl_dir}/*.erl")

    process_async(erl_paths, fn file ->
      IO.puts("Compiling #{file}")
      # debug_info is needed to disasseble the beam file later
      {_out, 0} = System.shell("erlc +debug_info -o #{out_dir} #{file}")
    end)

    File.rm_rf!(yrl_dir)

    :ok
  end

  @doc """
  Replaces functions in the stdlib with the AtomVM-specific
  implementations compiled with `compile/2`.
  """
  def patch(stdlib_beams, patch_beams, out_dir, opts \\ []) do
    [add_tracing: add_tracing] = Keyword.validate!(opts, add_tracing: false)

    (stdlib_beams ++ patch_beams)
    |> Enum.group_by(&Path.basename/1)
    |> process_async(fn
      # prim_eval fais to be parsed, probably because it's compiled from an asm (*.S) file
      # so we just copy it
      {name, [_estd_path, avm_path]} when name == "prim_eval.beam" ->
        File.cp!(avm_path, Path.join(out_dir, name))

      {name, paths} ->
        IO.puts("Patching #{name}")

        ast =
          case paths do
            [estd_path, avm_path] ->
              CoreErlangUtils.merge_modules(
                CoreErlangUtils.parse(File.read!(estd_path)),
                CoreErlangUtils.parse(File.read!(avm_path))
              )

            [path] ->
              CoreErlangUtils.parse(File.read!(path))
          end

        ast = if add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast

        beam = CoreErlangUtils.serialize(ast)
        File.write!(Path.join(out_dir, name), beam)
    end)

    :ok
  end

  defp process_async(enum, fun, opts \\ []) do
    enum
    |> Task.async_stream(fun, [timeout: 30_000, ordered: false] ++ opts)
    |> Stream.run()
  end
end
