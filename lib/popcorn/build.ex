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
        [:popcorn_lib | @available_apps],
        fn app ->
          app_cache = build_app(app, Map.get(cache, app, %{}))
          {app, app_cache}
        end,
        timeout: 600_000,
        max_concurrency: 2
      )
      |> Map.new()

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

  defp patchable_app_beams(:popcorn_lib), do: []

  defp patchable_app_beams(app) do
    app
    |> Application.app_dir()
    |> Path.join("ebin/**/*.beam")
    |> Path.wildcard()
  end

  defp patches_source(:popcorn_lib), do: Path.wildcard("patches/popcorn_lib/**/*.{ex,erl,yrl,S}")

  defp patches_source(app) do
    Path.wildcard("patches/{otp,elixir}/#{app}/*.{ex,erl,yrl,S}")
  end

  defp build_app(application, cache) do
    build_dir = bundle_ebin_dir(application)
    File.mkdir_p!(build_dir)

    app_beams = patchable_app_beams(application)

    # update cache hashes
    patches_srcs = patches_source(application)
    {modified_srcs, cache} = update_cache(patches_srcs, cache)

    {any_patched, remaining_beams} = patch(application, app_beams, modified_srcs, build_dir)
    any_copied = copy_beams(application, remaining_beams, build_dir)

    if any_patched or any_copied do
      Logger.info("Bundling #{application}.avm", app_name: application)
      create_bundle!(application)
    end

    cache
  end

  defp create_bundle!(app) do
    out_path = app |> bundle_path() |> to_charlist()
    beams = app |> bundle_beams() |> Enum.map(&String.to_charlist/1)

    :ok = :packbeam_api.create(out_path, beams)
  end

  defp bundle_beams(bundle) do
    bundle_ebin_dir(bundle)
    |> Path.join("*.beam")
    |> Path.wildcard()
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
  defp patch(app, beam_paths, patch_paths, out_dir) do
    beams_by_name = Map.new(beam_paths, &{Path.basename(&1), &1})

    # Compiling each file separately, like AtomVM does it.
    # Compiling together may break something, as these modules
    # will override stdlib modules.
    none_patched =
      process_async(patch_paths, fn patch_path ->
        with_tmp_dir(out_dir, fn tmp_dir ->
          Logger.info("Compiling #{patch_path}", app_name: app)

          patch_path
          |> compile_patch(tmp_dir)
          |> Enum.map(fn patch_beam_path ->
            name = Path.basename(patch_beam_path)
            do_patch(app, name, beams_by_name[name], patch_beam_path, out_dir)
            name
          end)
        end)
      end)
      |> Enum.empty?()

    # patch files may produce multiple beams
    # the easiest way to get full set is to diff input beams and beams already in out_dir
    out_dir_beams = Path.wildcard("#{out_dir}/*.beam") |> MapSet.new(&Path.basename/1)
    path_present? = fn path -> Path.basename(path) in out_dir_beams end
    remaining_beams = Enum.reject(beam_paths, path_present?)

    some_patched = not none_patched
    {some_patched, remaining_beams}
  end

  defp compile_patch(path, out_dir) do
    cmd =
      case Path.extname(path) do
        ".ex" ->
          "elixirc --ignore-module-conflict -o #{out_dir} #{path}"

        ".yrl" ->
          erl_path = "#{out_dir}/#{Path.basename(path, ".yrl")}.erl"

          "erlc -o #{out_dir} #{path} && erlc +debug_info -o #{out_dir} #{erl_path} && rm #{erl_path}"

        ext when ext in [".erl", ".S"] ->
          "erlc +debug_info -o #{out_dir} #{path}"
      end

    {_out, 0} = System.shell(cmd)
    Path.wildcard("#{out_dir}/*")
  end

  # prim_eval fails to be parsed, probably because it's compiled from an asm (*.S) file
  # so we just copy it
  defp do_patch(app_name, "prim_eval.beam" = name, _beam, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)

    File.cp!(patch, Path.join(out_dir, name))
  end

  # This is only needed to add tracing
  # but we execute it always for consistency
  # To be replaced with File.cp when we improve tracing in AtomVM
  defp do_patch(app_name, name, nil, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)

    transform_beam_ast(patch, out_dir, fn patch_ast ->
      maybe_add_tracing(patch_ast, @config.add_tracing)
    end)
  end

  defp do_patch(app_name, name, beam, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)
    patch_ast = CoreErlangUtils.parse(File.read!(patch))

    transform_beam_ast(beam, out_dir, fn ast ->
      CoreErlangUtils.merge_modules(ast, patch_ast)
      |> maybe_add_tracing(@config.add_tracing)
    end)
  end

  # This is only needed to add tracing
  # but we execute it always for consistency
  # To be removed when we improve tracing in AtomVM
  defp copy_beams(app_name, beams, out_dir) do
    none_copied =
      beams
      |> process_async(fn path ->
        name = Path.basename(path)
        Logger.info("Transferring #{name}", app_name: app_name)

        transform_beam_ast(path, out_dir, fn ast ->
          maybe_add_tracing(ast, @config.add_tracing)
        end)

        name
      end)
      |> Enum.empty?()

    some_copied = not none_copied
    some_copied
  end

  defp transform_beam_ast(beam_path, out_dir, transform) do
    name = Path.basename(beam_path)
    out_path = Path.join(out_dir, name)

    beam_path
    |> File.read!()
    |> CoreErlangUtils.parse()
    |> transform.()
    |> CoreErlangUtils.serialize()
    |> then(&File.write!(out_path, &1))
  end

  defp maybe_add_tracing(ast, true), do: CoreErlangUtils.add_simple_tracing(ast)
  defp maybe_add_tracing(ast, false), do: ast

  defp process_async(enum, fun, opts \\ []) do
    enum
    |> Task.async_stream(fun, Keyword.merge([timeout: 120_000, ordered: false], opts))
    |> Enum.map(fn {:ok, result} -> result end)
  end

  defp with_tmp_dir(path, f) do
    tmp_dir = Path.join(path, "tmp_#{:erlang.unique_integer([:positive])}")
    File.rm_rf!(tmp_dir)
    File.mkdir!(tmp_dir)

    try do
      f.(tmp_dir)
    after
      File.rm_rf!(tmp_dir)
    end
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
