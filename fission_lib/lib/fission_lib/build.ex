defmodule FissionLib.Build do
  @moduledoc false
  require FissionLib.Config
  alias FissionLib.CoreErlangUtils

  @config FissionLib.Config.get([:add_tracing, :ex_stdlib_beam_paths, :erl_stdlib_beam_paths])

  @app_path Mix.Project.app_path()
  @build_path "#{@app_path}/flb_patches_ebin"
  @cache_path "#{@app_path}/flb_patch_cache"

  # When this module is recompiled, we need to
  # patch from scratch
  File.rm(@cache_path)

  @doc """
  Runs `compile/2` and `patch/3`, then packs output beams
  into a single *.avm file.
  """
  def build(opts \\ []) do
    opts = opts |> Keyword.validate!(out_dir: @app_path, force: false) |> Map.new()
    stdlib_beam_paths = @config.ex_stdlib_beam_paths ++ @config.erl_stdlib_beam_paths
    patches_srcs = Path.wildcard("patches/**/*.{ex,erl,yrl,S}")

    cache =
      with false <- opts.force,
           {:ok, cache} <- File.read(@cache_path),
           cache = :erlang.binary_to_term(cache),
           # If a patch was removed, we need to remove the old build
           # to avoid adding stale artifacts to the bundle
           missing_srcs = Map.drop(cache, patches_srcs),
           true <- Enum.empty?(missing_srcs) do
        cache
      else
        _cant_use_cache ->
          File.rm_rf!(@cache_path)
          File.rm_rf!(@build_path)
          File.mkdir_p!(@build_path)
          %{}
      end

    {patches_srcs, cache} = handle_cache(patches_srcs, cache)

    patch(stdlib_beam_paths, patches_srcs, @build_path)
    transfer_stdlib(stdlib_beam_paths, @build_path)

    IO.puts("Bundling fission_lib.avm")

    :packbeam_api.create(
      ~c"#{opts.out_dir}/fission_lib.avm",
      Path.wildcard("#{@build_path}/*.beam") |> Enum.map(&String.to_charlist/1)
    )

    File.write!(@cache_path, :erlang.term_to_binary(cache))

    :ok
  end

  defp handle_cache(paths, cache) do
    Enum.flat_map_reduce(paths, %{}, fn path, new_cache ->
      hash = path |> File.read!() |> :erlang.md5()
      new_cache = Map.put(new_cache, path, hash)

      if hash == cache[path] do
        {[], new_cache}
      else
        {[path], new_cache}
      end
    end)
  end

  # Compiles and applies patches to Erlang and Elixir standard libraries,
  # so they could work with the AtomVM.
  # The patching works by replacing original functions implementations
  # with custom ones.
  # The sources of the patches reside in the `patches` directory.
  defp patch(stdlib_beams, patch_srcs, out_dir) do
    stdlib_beams = Map.new(stdlib_beams, &{Path.basename(&1), &1})

    # Compiling each file separately, like AtomVM does it.
    # Compiling together may break something, as these modules
    # will override stdlib modules.
    process_async(patch_srcs, fn src ->
      with_tmp_dir(out_dir, fn tmp_dir ->
        IO.puts("Compiling #{src}")
        compile_patch(src, tmp_dir)

        Path.wildcard("#{tmp_dir}/*")
        |> Enum.map(fn path ->
          name = Path.basename(path)
          do_patch(name, stdlib_beams[name], path, out_dir)
          name
        end)
      end)
    end)
  end

  defp compile_patch(path, tmp_dir) do
    cmd =
      case Path.extname(path) do
        ".ex" ->
          "elixirc --ignore-module-conflict -o #{tmp_dir} #{path}"

        ".yrl" ->
          erl_path = "#{tmp_dir}/#{Path.basename(path, ".yrl")}.erl"

          "erlc -o #{tmp_dir} #{path} && erlc +debug_info -o #{tmp_dir} #{erl_path} && rm #{erl_path}"

        ext when ext in [".erl", ".S"] ->
          "erlc +debug_info -o #{tmp_dir} #{path}"
      end

    {_out, 0} = System.shell(cmd)
    :ok
  end

  # prim_eval fais to be parsed, probably because it's compiled from an asm (*.S) file
  # so we just copy it
  defp do_patch("prim_eval.beam" = name, _stdlib, patch, out_dir) do
    File.cp!(patch, Path.join(out_dir, name))
  end

  # This is only needed to add tracing
  # but we execute it always for consistency
  # To be replaced with File.cp when we improve tracing in AtomVM
  defp do_patch(name, nil, patch, out_dir) do
    IO.puts("Patching #{name}")
    ast = File.read!(patch) |> CoreErlangUtils.parse()
    ast = if @config.add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast
    beam = CoreErlangUtils.serialize(ast)
    File.write!(Path.join(out_dir, name), beam)
  end

  defp do_patch(name, stdlib, patch, out_dir) do
    IO.puts("Patching #{name}")

    ast =
      CoreErlangUtils.merge_modules(
        CoreErlangUtils.parse(File.read!(stdlib)),
        CoreErlangUtils.parse(File.read!(patch))
      )

    ast = if @config.add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast
    beam = CoreErlangUtils.serialize(ast)
    File.write!(Path.join(out_dir, name), beam)
  end

  # This is only needed to add tracing
  # but we execute it always for consistency
  # To be removed when we improve tracing in AtomVM
  defp transfer_stdlib(stdlib_beams, out_dir) do
    patched_beams = Path.wildcard("#{out_dir}/*") |> MapSet.new(&Path.basename/1)

    stdlib_beams
    |> Enum.reject(&(Path.basename(&1) in patched_beams))
    |> process_async(fn path ->
      name = Path.basename(path)
      IO.puts("Transferring #{name}")
      ast = File.read!(path) |> CoreErlangUtils.parse()
      ast = if @config.add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast
      beam = CoreErlangUtils.serialize(ast)
      File.write!(Path.join(out_dir, name), beam)
    end)
  end

  defp process_async(enum, fun, opts \\ []) do
    enum
    |> Task.async_stream(fun, [timeout: 30_000, ordered: false] ++ opts)
    |> Stream.run()
  end

  defp with_tmp_dir(path, fun) do
    tmp_dir = "#{path}/tmp_#{:erlang.unique_integer([:positive])}"
    File.rm_rf!(tmp_dir)
    File.mkdir!(tmp_dir)
    result = fun.(tmp_dir)
    File.rm_rf!(tmp_dir)
    result
  end
end
