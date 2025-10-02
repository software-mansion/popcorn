defmodule Popcorn.Build do
  @moduledoc false
  require Popcorn.Config
  require Logger
  alias Popcorn.CoreErlangUtils
  alias Popcorn.BuildLogFormatter
  alias Popcorn.ArtifactsCache

  @after_compile __MODULE__
  def __after_compile__(_env, _bytecode) do
    # When this module is recompiled, we need to
    # patch from scratch
    ArtifactsCache.drop_cache!()
  end

  @config Popcorn.Config.compile([:add_tracing, :extra_apps])

  @app_path Mix.Project.app_path()
  @patches_path "#{@app_path}/popcorn_patches"

  # A minimal set of apps to start Popcorn-based app
  @default_apps [
    # OTP
    :compiler,
    :erts,
    :kernel,
    :stdlib,
    # Elixir
    :elixir,
    :logger
  ]

  @available_apps @default_apps ++ @config.extra_apps

  @doc """
  Applies patches and generates *.avm bundles for each (configured)
  OTP and Elixir application, as well as `popcorn_lib.avm`
  with all additional BEAMs
  """
  def build() do
    Logger.info("Bundling started")
    start_time = DateTime.utc_now()
    original_formatter = BuildLogFormatter.enable()

    patches_srcs = Path.wildcard("patches/**/*.{ex,erl,yrl,S}") |> MapSet.new()
    cache = ArtifactsCache.read_from_disk!()

    cache =
      if not ArtifactsCache.subset_of_sources?(cache, patches_srcs) do
        File.rm_rf!(@patches_path)
        File.mkdir_p!(@patches_path)
        %{}
      else
        cache
      end

    ArtifactsCache.drop_cache!()

    new_cache =
      process_async(
        @available_apps,
        fn app ->
          app_cache = build_app(app, Map.get(cache, app, %{}))
          {app, app_cache}
        end,
        timeout: 600_000,
        max_concurrency: 2
      )
      |> Map.new()

    popcorn_lib_cache = build_popcorn(cache[:popcorn_lib])
    new_cache = Map.put(new_cache, :popcorn_lib, popcorn_lib_cache)

    ArtifactsCache.write_to_disk!(new_cache)

    :ok
    BuildLogFormatter.disable(original_formatter)
    end_time = DateTime.utc_now()
    duration = DateTime.diff(end_time, start_time, :microsecond)
    Logger.info("Bundling finished (took #{to_human_duration(duration)})")
  end

  @doc """
  Returns a list of names of applications shipped with Erlang/OTP and Elixir
  """
  @spec builtin_app_names() :: [String.t()]
  def builtin_app_names() do
    otp_apps =
      :code.lib_dir()
      |> IO.chardata_to_string()
      |> File.ls!()
      |> Enum.map(fn app_dir ->
        app_dir
        |> String.split("-")
        |> hd()
      end)

    elixir_apps =
      Path.expand("..", Application.app_dir(:elixir))
      |> File.ls!()

    otp_apps ++ elixir_apps
  end

  @doc """
  Returns a list of applications that were included during compilation
  and their bundle is available at `bundle_path/0`
  """
  @spec available_apps() :: [atom()]
  def available_apps() do
    @available_apps
  end

  @doc """
  Returns a path to an AVM bundle with the provided name
  """
  def bundle_path(bundle), do: Path.join([@patches_path, "#{bundle}.avm"])

  defp bundle_ebin_dir(bundle), do: Path.join([@patches_path, to_string(bundle), "ebin"])

  defp bundle_beams(bundle) do
    bundle_ebin_dir(bundle)
    |> Path.join("*.beam")
    |> Path.wildcard()
    |> Enum.map(&String.to_charlist/1)
  end

  defp build_app(application, cache) do
    build_dir = bundle_ebin_dir(application)
    File.mkdir_p!(build_dir)

    app_beams =
      application
      |> Application.app_dir()
      |> Path.join("ebin/**/*.beam")
      |> Path.wildcard()

    patches_srcs = Path.wildcard("patches/{otp,elixir}/#{application}/*.{ex,erl,yrl,S}")

    {modified_srcs, cache} = update_cache(patches_srcs, cache)

    patched_beams = patch(application, app_beams, modified_srcs, build_dir)

    transferred_beams = transfer_stdlib(application, app_beams, build_dir)

    if patched_beams != [] or transferred_beams != [] do
      Logger.info("Bundling #{application}.avm", app_name: application)
      :packbeam_api.create(to_charlist(bundle_path(application)), bundle_beams(application))
    end

    cache
  end

  defp build_popcorn(cache) do
    bundle = :popcorn_lib
    build_dir = bundle_ebin_dir(bundle)
    File.mkdir_p!(build_dir)
    srcs = Path.wildcard("patches/#{bundle}/**/*.{ex,erl,yrl,S}")

    {modified_srcs, cache} = update_cache(srcs, cache)

    popcorn_lib_beams = patch(bundle, [], modified_srcs, build_dir)

    if popcorn_lib_beams != [] do
      Logger.info("Bundling #{bundle}.avm", app_name: bundle)
      :packbeam_api.create(to_charlist(bundle_path(bundle)), bundle_beams(bundle))
    end

    cache
  end

  # Updates hash of each patch and returns only paths that have different hash
  defp update_cache(paths, cache) do
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
  defp patch(app, stdlib_beams, patch_srcs, out_dir) do
    stdlib_beams_by_name = Map.new(stdlib_beams, &{Path.basename(&1), &1})

    # Compiling each file separately, like AtomVM does it.
    # Compiling together may break something, as these modules
    # will override stdlib modules.
    process_async(patch_srcs, fn src ->
      tmp_dir = setup_tmp_dir(out_dir)

      try do
        Logger.info("Compiling #{src}", app_name: app)
        compile_patch(src, tmp_dir)

        Path.wildcard("#{tmp_dir}/*")
        |> Enum.map(fn path ->
          name = Path.basename(path)
          do_patch(app, name, stdlib_beams_by_name[name], path, out_dir)
          name
        end)
      after
        File.rm_rf!(tmp_dir)
      end
    end)
    |> List.flatten()
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

  # prim_eval fails to be parsed, probably because it's compiled from an asm (*.S) file
  # so we just copy it
  defp do_patch(app_name, "prim_eval.beam" = name, _stdlib, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)

    File.cp!(patch, Path.join(out_dir, name))
  end

  # This is only needed to add tracing
  # but we execute it always for consistency
  # To be replaced with File.cp when we improve tracing in AtomVM
  defp do_patch(app_name, name, nil, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)
    ast = File.read!(patch) |> CoreErlangUtils.parse()
    ast = if @config.add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast
    beam = CoreErlangUtils.serialize(ast)
    File.write!(Path.join(out_dir, name), beam)
  end

  defp do_patch(app_name, name, stdlib, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)

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
  defp transfer_stdlib(app_name, stdlib_beams, out_dir) do
    already_transferred = MapSet.new(File.ls!(out_dir))

    stdlib_beams
    |> Enum.reject(&(Path.basename(&1) in already_transferred))
    |> process_async(fn path ->
      name = Path.basename(path)
      Logger.info("Transferring #{name}", app_name: app_name)
      ast = File.read!(path) |> CoreErlangUtils.parse()
      ast = if @config.add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast
      beam = CoreErlangUtils.serialize(ast)
      File.write!(Path.join(out_dir, name), beam)
      name
    end)
  end

  defp process_async(enum, fun, opts \\ []) do
    enum
    |> Task.async_stream(fun, Keyword.merge([timeout: 120_000, ordered: false], opts))
    |> Enum.map(fn {:ok, result} -> result end)
  end

  defp setup_tmp_dir(path) do
    tmp_dir = Path.join(path, "tmp_#{:erlang.unique_integer([:positive])}")
    File.rm_rf!(tmp_dir)
    File.mkdir!(tmp_dir)
    tmp_dir
  end

  defp to_human_duration(us) do
    us = :erlang.float(us)
    ms = us / 1_000
    s = ms / 1_000

    cond do
      s > 1.0 -> "#{Float.round(s, 2)}s"
      ms > 1.0 -> "#{Float.round(ms, 2)}ms"
      true -> "#{Float.round(us, 2)}us"
    end
  end
end
