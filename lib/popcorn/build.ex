defmodule Popcorn.Build do
  @moduledoc """
  Uses system OTP and Elixir applications to create .avm bundles used in user bundles.
  Applies Popcorn patches to .beam files in the applications.
  """
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
    original_formatter = BuildLogFormatter.enable()

    cache = ArtifactsCache.read_from_disk!()
    new_cache = create_new_cache([:popcorn_lib | @available_apps])
    modified = ArtifactsCache.get_modified_apps(cache, new_cache)

    # in case of any raise later
    # we possibly have corrupted cache and want to start all over
    ArtifactsCache.drop_cache!()

    File.mkdir_p!(@patches_path)
    # ensure that we start with clean state for modified apps
    modified
    |> Map.keys()
    |> process_async(&delete_built_artifacts/1)

    process_async(
      modified,
      fn {modified_app, modified_paths} ->
        build_app(modified_app, modified_paths)
      end,
      timeout: 600_000,
      max_concurrency: 2
    )

    updated_cache = Map.merge(cache, new_cache)
    ArtifactsCache.write_to_disk!(updated_cache)

    BuildLogFormatter.disable(original_formatter)
    Logger.info("Bundling finished")
    :ok
  end

  defp create_new_cache(apps) do
    apps
    |> process_async(fn app -> {app, compute_file_hashes(app)} end)
    |> Map.new()
  end

  defp compute_file_hashes(app) do
    case app do
      :popcorn_lib -> "patches/popcorn_lib/**/*.{ex,erl,yrl,S}"
      app -> "patches/{otp,elixir}/#{app}/*.{ex,erl,yrl,S}"
    end
    |> Path.wildcard()
    |> process_async(fn path ->
      hash = path |> File.read!() |> :erlang.md5()

      {path, hash}
    end)
    |> Map.new()
  end

  defp delete_built_artifacts(app) do
    build_dir = bundle_ebin_dir(app)
    bundle_path = bundle_path(app)

    # we don't care about failure
    Enum.each([build_dir, bundle_path], &File.rm/1)
    File.mkdir_p!(build_dir)
  end

  @doc """
  Returns a list of names of applications shipped with Erlang/OTP and Elixir
  """
  @spec builtin_app_names() :: [String.t()]
  def builtin_app_names() do
    otp_apps =
      :code.lib_dir()
      |> to_string()
      |> File.ls!()
      |> Enum.map(fn app_dir ->
        [app_name, _version] = String.split(app_dir, "-", parts: 2)
        app_name
      end)

    elixir_apps = Path.expand("..", Application.app_dir(:elixir)) |> File.ls!()

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

  defp build_app(application, modified_srcs) do
    build_dir = bundle_ebin_dir(application)
    File.mkdir_p!(build_dir)

    app_beams = patchable_app_beams(application)
    {any_patched, remaining_beams} = patch(application, app_beams, modified_srcs, build_dir)
    any_copied = copy_beams(application, remaining_beams, build_dir)

    if any_patched or any_copied do
      Logger.info("Bundling #{application}.avm", app_name: application)
      create_bundle!(application)
    end
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
          # credo:disable-for-lines:3 Credo.Check.Refactor.Nesting
          |> Enum.map(fn patch_beam_path ->
            name = Path.basename(patch_beam_path)
            do_patch(app, name, beams_by_name[name], patch_beam_path, out_dir)
          end)
        end)
      end)
      |> Enum.empty?()

    # patch files may produce multiple beams
    # the easiest way to get full set is to diff input beams and beams already in out_dir
    out_dir_beams = Path.wildcard("#{out_dir}/*.beam") |> MapSet.new(&Path.basename/1)
    path_present? = fn path -> Path.basename(path) in out_dir_beams end
    remaining_beams = Enum.reject(beam_paths, path_present?)

    {not none_patched, remaining_beams}
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

    not none_copied
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
end
