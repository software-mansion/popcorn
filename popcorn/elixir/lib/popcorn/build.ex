defmodule Popcorn.Build do
  @moduledoc false
  # Uses system OTP and Elixir applications to create .avm bundles used in user bundles.
  # Applies Popcorn patches to .beam files in the applications.

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
  @src_wildcard "*.{ex,erl,yrl,S}"

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

    patches_srcs = Path.wildcard("patches/**/#{@src_wildcard}") |> MapSet.new()
    cache = ArtifactsCache.read_from_disk!()
    is_cache_invalid = not ArtifactsCache.subset_of_sources?(cache, patches_srcs)

    cache =
      if is_cache_invalid do
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

    BuildLogFormatter.disable(original_formatter)
    end_time = DateTime.utc_now()
    duration = DateTime.diff(end_time, start_time, :microsecond)
    Logger.info("Bundling finished (took #{to_human_duration(duration)})")
    :ok
  end

  @doc """
  Returns a list of names of applications shipped with Erlang/OTP and Elixir
  """
  @spec builtin_apps() :: [atom()]
  def builtin_apps() do
    otp_apps =
      :code.lib_dir()
      |> to_string()
      |> File.ls!()
      |> Enum.map(fn app_dir ->
        [app_name, _version] = String.split(app_dir, "-", parts: 2)
        app_name
      end)

    elixir_apps = Path.expand("..", Application.app_dir(:elixir)) |> File.ls!()

    Enum.map(otp_apps ++ elixir_apps, &String.to_atom/1)
  end

  @doc """
  Returns a list of applications that were included during compilation
  """
  @spec available_apps() :: [atom()]
  def available_apps() do
    @available_apps
  end

  def patched_ebin_dir(app), do: Path.join([@patches_path, to_string(app), "ebin"])

  defp patchable_app_ebins(:popcorn_lib), do: []

  defp patchable_app_ebins(app) do
    app
    |> Application.app_dir()
    |> Path.join("ebin/*")
    |> Path.wildcard()
  end

  defp patches_source(:popcorn_lib), do: Path.wildcard("patches/popcorn_lib/**/#{@src_wildcard}")

  defp patches_source(app) do
    Path.wildcard("patches/{otp,elixir}/#{app}/#{@src_wildcard}")
  end

  defp build_app(application, cache) do
    build_dir = patched_ebin_dir(application)
    File.mkdir_p!(build_dir)

    app_ebins = patchable_app_ebins(application)

    # update cache hashes
    patches_srcs = patches_source(application)
    {modified_srcs, cache} = update_cache(patches_srcs, cache)

    remaining_ebins = patch(application, app_ebins, modified_srcs, build_dir)
    copy_ebins(application, remaining_ebins, build_dir)

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
  defp patch(app, ebin_paths, patch_paths, out_dir) do
    ebins_by_name = Map.new(ebin_paths, &{Path.basename(&1), &1})

    patch_associated_beam = fn patch_path, tmp_dir ->
      Logger.info("Compiling #{patch_path}", app_name: app)

      patch_path
      |> compile_patch(tmp_dir)
      |> Enum.map(fn patch_beam_path ->
        name = Path.basename(patch_beam_path)
        do_patch(app, name, ebins_by_name[name], patch_beam_path, out_dir)
      end)
    end

    # Compiling each file separately, like AtomVM does it.
    # Compiling together may break something, as these modules
    # will override stdlib modules.
    process_async(patch_paths, fn patch_path ->
      with_tmp_dir(out_dir, fn tmp_dir ->
        patch_associated_beam.(patch_path, tmp_dir)
      end)
    end)

    # patch files may produce multiple beams
    # the easiest way to get full set is to diff input ebins and beams already in out_dir
    out_dir_ebins = Path.wildcard("#{out_dir}/*") |> MapSet.new(&Path.basename/1)

    path_present? = fn path -> Path.basename(path) in out_dir_ebins end
    remaining_ebins = Enum.reject(ebin_paths, path_present?)

    remaining_ebins
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
    Logger.info("Transferring #{name}", app_name: app_name)

    if @config.add_tracing and Path.extname(patch) == ".beam" do
      transform_beam_ast(patch, out_dir, fn ast -> CoreErlangUtils.add_simple_tracing(ast) end)
    else
      File.cp!(patch, Path.join(out_dir, name))
    end
  end

  defp do_patch(app_name, name, beam, patch, out_dir) do
    Logger.info("Patching #{name}", app_name: app_name)
    patch_ast = CoreErlangUtils.parse(File.read!(patch))

    transform_beam_ast(beam, out_dir, fn ast ->
      ast = CoreErlangUtils.merge_modules(ast, patch_ast)
      if @config.add_tracing, do: CoreErlangUtils.add_simple_tracing(ast), else: ast
    end)
  end

  # This is only needed to add tracing
  # but we execute it always for consistency
  # To be removed when we improve tracing in AtomVM
  defp copy_ebins(app_name, ebins, out_dir) do
    process_async(ebins, fn path ->
      name = Path.basename(path)
      Logger.info("Transferring #{name}", app_name: app_name)

      if @config.add_tracing and Path.extname(path) == ".beam" do
        transform_beam_ast(path, out_dir, fn ast -> CoreErlangUtils.add_simple_tracing(ast) end)
      else
        File.cp!(path, Path.join(out_dir, name))
      end
    end)

    :ok
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
