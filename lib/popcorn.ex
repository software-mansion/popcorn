defmodule Popcorn do
  @moduledoc """
  Popcorn is a tool for running Elixir in the browser.
  """

  alias Popcorn.Utils.FetchArtifacts

  @app_build_root Mix.Project.build_path()
  @popcorn_path Mix.Project.app_path()
  @popcorn_bundle_path Path.join(@popcorn_path, "popcorn.avm")
  @popcorn_generated_path Path.join(@popcorn_path, "popcorn_generated_ebin")
  @priv_dir :code.priv_dir(:popcorn)
  @api_dir Path.join(["popcorn", "api"])

  defmodule CookingError do
    @moduledoc false
    defexception [:message]
  end

  @doc """
  Generates static artifacts to run the project in the browser.

  `out_dir` and `start_module` are mandatory.

  Options:
  - `out_dir` - the directory to write artifacts to
  - `start_module` - a module with `start/0` function, used by AtomVM as starting point
  - `target` - `wasm` (default) or `unix`. If `unix` is chosed, you need to build the runtime
  first with `mix popcorn.build_runtime --target unix`
  - `compile_artifacts` - compiled BEAMs and other artifacts that should be included
  in the generated bundle. Defaults to all the `.beam` and `.app` files for the application
  and dependencies.
  """
  @spec cook([
          {:out_dir, String.t()}
          | {:start_module, module}
          | {:target, :wasm | :unix}
          | {:compile_artifacts, [String.t()]}
        ]) :: :ok
  def cook(options \\ []) do
    ingredients(Keyword.take(options, [:out_dir, :target]))
    bundle(Keyword.take(options, [:out_dir, :start_module, :compile_artifacts]))
  end

  def bundle(options \\ []) do
    {module, filename} = create_boot_module(options[:start_module])

    try do
      all_beams = Path.wildcard(Path.join([@app_build_root, "**", "*.{beam,app}"]))

      default_options = [
        out_dir: Popcorn.Config.get(:out_dir),
        start_module: nil,
        compile_artifacts: all_beams
      ]

      options = options |> Keyword.validate!(default_options) |> Map.new()
      ensure_option_present(options, :out_dir, "Output directory")

      File.mkdir_p!(options.out_dir)

      bundled_artifacts = bundled_artifacts([filename | options.compile_artifacts])

      pack_result = pack_bundle(options.out_dir, bundled_artifacts, module)

      case pack_result do
        :ok -> :ok
        {:error, reason} -> raise CookingError, "Reason: #{inspect(reason)}"
      end
    after
      :ok = File.rm!(filename)
    end
  end

  def ingredients(options) do
    default_options = [
      out_dir: Popcorn.Config.get(:out_dir),
      target: :wasm
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()
    ensure_option_present(options, :out_dir, "Output directory")

    File.mkdir_p!(options.out_dir)
    copy_runtime_artifacts(options)
    :ok
  end

  defp bundled_artifacts(compile_artifacts) do
    popcorn_files = Path.wildcard(Path.join([@popcorn_path, "**", "*"]))
    api_beams = Enum.filter(popcorn_files, &popcorn_api_beam?/1)
    generated_beams = Path.wildcard(Path.join([@popcorn_generated_path, "*.beam"]))

    # include stdlib bundle, Popcorn.Wasm beam and filter other popcorn beams
    [@popcorn_bundle_path | api_beams] ++ generated_beams ++ (compile_artifacts -- popcorn_files)
  end

  defp pack_bundle(out_dir, beams, start_module) do
    bundle_path = Path.join(out_dir, "bundle.avm")

    # packbeam appends to the bundle if output exists, we need to delete it first
    maybe_rm(bundle_path)
    bundle_path = to_charlist(bundle_path)
    beams = Enum.map(beams, &String.to_charlist/1)

    try do
      :packbeam_api.create(bundle_path, beams, %{start_module: start_module})
      gzip(bundle_path)
    catch
      {:start_module_not_found, _module} ->
        raise CookingError, "Provided start module `#{inspect(start_module)}` has not been found"
    end
  end

  defp maybe_rm(path) do
    case File.rm(path) do
      :ok -> :ok
      {:error, :enoent} -> :ok
      error -> raise CookingError, "Couldn't remove old bundle, reason: #{inspect(error)}"
    end
  end

  defp popcorn_api_beam?(path) do
    in_popcorn_ebin_dir?(path) and beam?(path) and beam_src_in_api_dir?(path)
  end

  defp in_popcorn_ebin_dir?(path) do
    dir = path |> Path.relative_to(@popcorn_path) |> Path.dirname()
    dir == "ebin"
  end

  defp beam?(path), do: Path.extname(path) == ".beam"

  defp beam_src_in_api_dir?(beam_path) do
    module_name = Path.basename(beam_path, ".beam")

    module =
      if String.starts_with?(module_name, "Elixir.") do
        Module.safe_concat([module_name])
      else
        String.to_existing_atom(module_name)
      end

    src_path = to_string(module.module_info(:compile)[:source])

    String.contains?(src_path, @api_dir)
  end

  defp copy_runtime_artifacts(options) do
    if options.target == :wasm do
      wasm_template_files = Path.wildcard(Path.join(@priv_dir, "static-template/wasm/**"))
      cp_gzip(wasm_template_files, options.out_dir)
    end

    paths = FetchArtifacts.fetch_artifacts(options.target)

    cp_gzip(paths, options.out_dir, fn path ->
      raise CookingError, """
      Couldn't find runtime artifact #{Path.basename(path)} at #{path} for target `#{options.target}`. \
      To build artifacts from source, run \
      `mix popcorn.build_runtime --target #{options.target}`.
      """
    end)
  end

  defp ensure_option_present(options, key, name) do
    if options[key] == nil do
      raise CookingError, """
      #{name} not provided. \
      Please provide the `#{key}` option or configure it by putting \
      `config :popcorn, #{key}: value` in your `config.exs`.
      """
    end
  end

  defp create_boot_module(start_module) do
    config = Mix.Project.config()
    app = Keyword.get(config, :app)

    all_specs = gather_app_specs([:kernel, :stdlib, app], %{})

    # FIXME: logger app is broken for now, skip it
    specs =
      all_specs
      |> Map.delete(:logger)
      |> Map.new(fn {app, spec} ->
        spec =
          Keyword.update(spec, :applications, [], &Enum.reject(&1, fn x -> x == :logger end))

        {app, spec}
      end)

    if all_specs != specs do
      env = __ENV__

      # Intentionally omit line number and whole stacktrace plus ensure file is relative to popcorn root
      IO.warn(
        "Disabled unsupported :logger application, some calls may break at runtime",
        file: Path.relative_to(env.file, Path.expand(__DIR__ <> "../../..")),
        module: __MODULE__,
        function: env.function
      )
    end

    # TODO: Until separation of deps for tasks and runtime is solved, disable all extra apps from :popcorn
    specs =
      if Map.has_key?(specs, :popcorn) do
        put_in(specs[:popcorn][:applications], [:kernel, :stdlib, :elixir])
      else
        specs
      end

    # Ensure shell_history is disabled as it will cause crash due to unimplemented IO & others
    specs = put_in(specs[:kernel][:env][:shell_history], :disabled)

    run =
      if start_module != nil do
        quote do
          unquote(start_module).start()
        end
      else
        quote do
          case :application.get_supervisor(unquote(app)) do
            :undefined ->
              :ok

            {:ok, pid} ->
              ref = Process.monitor(pid)

              receive do
                {:DOWN, ^ref, :process, _object, reason} ->
                  reason
              end
          end
        end
      end

    no_warn_undefined = [:atomvm | List.wrap(start_module)]

    # The module below tries to mimic BEAMs boot script, then start user's app
    contents =
      quote location: :keep do
        @compile autoload: false, no_warn_undefined: unquote(no_warn_undefined)

        def start() do
          # TODO: Default boot script starts `:heart` process, but unless -heart flag is passed, it will return `:ignore`
          # :ignore = :heart.start()
          # TODO: Default boot script starts :logger_server, uncomment line below when :logger app is supported
          # {:ok, _pid} = :logger_server.start_link()

          specs = unquote(Macro.escape(specs))

          {:ok, _ac} =
            :application_controller.start({:application, :kernel, specs[:kernel]})

          for {app, spec} <- specs, app != :kernel do
            :ok = :application.load({:application, app, spec})
          end

          :ok = :application.start_boot(:kernel, :permanent)
          :ok = :application.start_boot(:stdlib, :permanent)

          {:ok, _apps} = Application.ensure_all_started(unquote(app), :permanent)

          unquote(run)
        end
      end

    module_name = Popcorn.Boot

    {:module, _module_name, binary_content, _term} =
      Module.create(module_name, contents, Macro.Env.location(__ENV__))

    File.mkdir_p!(@popcorn_generated_path)
    filename = Path.join(@popcorn_generated_path, "#{module_name}.beam")
    File.write!(filename, binary_content)
    {module_name, filename}
  end

  defp gather_app_specs([], specs), do: specs

  defp gather_app_specs(apps, specs) do
    new_apps = Enum.reject(apps, &Map.has_key?(specs, &1))

    new_specs =
      for app <- new_apps,
          # Missing optional apps (`nil` spec) will be filtered out
          spec = Application.spec(app),
          into: %{} do
        env = Application.get_all_env(app)
        {app, [env: env] ++ spec}
      end

    deps =
      new_specs
      |> Enum.flat_map(fn
        {_app, spec} -> spec[:applications]
      end)
      |> Enum.uniq()

    gather_app_specs(deps, Map.merge(specs, new_specs))
  end

  defp cp_gzip(paths, out_dir, handle_missing_file \\ fn _path -> :ok end) do
    for path <- List.wrap(paths) do
      dest = Path.join(out_dir, Path.basename(path))
      if not File.exists?(path), do: handle_missing_file.(path)
      File.cp!(path, dest)
      gzip(dest)
    end

    :ok
  end

  defp gzip(path) do
    File.read!(path) |> :zlib.gzip() |> then(&File.write!("#{path}.gz", &1))
  end
end
