defmodule Popcorn do
  @moduledoc """
  Popcorn is a tool for running Elixir in the browser.
  """

  alias Popcorn.Build

  @popcorn_path Mix.Project.app_path()
  @popcorn_generated_path Path.join(@popcorn_path, "popcorn_generated_ebin")
  @popcorn_treeshaked_path Path.join(@popcorn_path, "popcorn_treeshaked_ebin")
  @api_dir Path.join(["popcorn", "api"])

  defmodule CookingError do
    @moduledoc false
    defexception [:message]
  end

  @doc """
  Builds a Popcorn `.avm` bundle.

  Options:
  - `out_dir` - The directory to write artifacts to. Required, unless provided via `config.exs`.
  - `start_module` - Optional; a module with `start/0` function that will be called after applications start.
  - `extra_beams` - Compiled BEAMs that should be included in the generated bundle.
  - `treeshake` - [Experimental] When `true`, removes unused modules and functions to reduce bundle size.
    Also removes location data (files and line numbers), which results in less useful stack traces.
    Defaults to `false`.
  """
  @spec cook([
          {:out_dir, String.t()}
          | {:start_module, module}
          | {:extra_beams, [String.t()]}
          | {:treeshake, boolean()}
        ]) :: :ok
  def cook(options \\ []) do
    default_options = [
      out_dir: Popcorn.Config.get(:out_dir),
      treeshake: Popcorn.Config.get(:treeshake),
      start_module: nil,
      extra_beams: []
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()
    ensure_option_present(options, :out_dir, "Output directory")

    File.mkdir_p!(options.out_dir)
    app = Mix.Project.config() |> Keyword.get(:app)
    apps_specs = gather_app_specs([:kernel, :stdlib, app], %{})
    apps = Map.keys(apps_specs)
    appconfig_module_path = create_appconfig_module(app, options.start_module, apps_specs)

    try do
      ebins = options.extra_beams ++ get_all_ebins(apps)
      ebins = if options.treeshake, do: treeshake(ebins, options.start_module), else: ebins
      beams = Enum.filter(ebins, &(Path.extname(&1) == ".beam"))
      pack_bundle(options.out_dir, beams, options.treeshake)
    after
      File.rm!(appconfig_module_path)
    end
  end

  defp treeshake(ebin_files, start_module) do
    File.rm_rf!(@popcorn_treeshaked_path)
    File.mkdir!(@popcorn_treeshaked_path)

    start_fun = if start_module, do: [{start_module, :start, 0}], else: []

    opts = [
      ebin_files: ebin_files,
      # verbose: true,
      output_dir: @popcorn_treeshaked_path,
      # stub_removed_functions: true,
      keep: [
        Popcorn.Boot,
        %{behaviour_impls: LocalLiveView}
        | start_fun
      ],
      ignore: [
        # AppConfig is ignored because it contains hardcoded list of all modules
        # of all apps, making treeshake think they're referenced, while they aren't
        Popcorn.AppConfig,
        # TODO application_controller references a lot of code that it doesn't use,
        # we need to figure out if we can avoid keeping it
        :application_controller
      ],
      leave: [
        Popcorn.AppConfig,
        :application_controller
      ],
      drop: [
        Code.Formatter,
        :elixir_parser,
        :elixir_tokenizer,
        :erl_lint,
        :erl_parse,
        :erl_eval,
        # TODO why logger needs these?
        # :epp,
        # :erl_scan,
        :prim_inet,
        :qlc,
        :qlc_pt,
        :dets_v9,
        :dets,
        :sofs,
        :erl_tar,
        :file_sorter,
        :global,
        :disk_log,
        :disk_log_1,
        :net_kernel,
        :zip,
        :inet_db,
        :shell,
        :edlin,
        :edlin_expand,
        :edlin_type_suggestion,
        :edlin_context,
        :dets_utils,
        :gen_tcp_socket,
        :socket,
        :prim_socket,
        :eval_bits,
        :inet_dns,
        :net,
        :rpc,
        :gen_udp_socket,
        :dist_util,
        :win32reg
      ]
    ]

    stats = Treeshake.run(opts)
    File.mkdir_p!("call_graph")
    File.write!("call_graph/.gitignore", "*")
    File.write!("call_graph/cg.bin", :erlang.term_to_binary(stats.call_graph))

    ls_paths(@popcorn_treeshaked_path)
  end

  defp get_all_ebins(applications) do
    builtin_apps = Build.builtin_apps() |> MapSet.new()
    builtin_deps = Enum.filter(applications, &(&1 in builtin_apps))

    case builtin_deps -- Build.available_apps() do
      [] ->
        :ok

      missing ->
        raise CookingError, """
        Popcorn was built without the following apps: #{inspect(missing)}
        Add them to :extra_apps in your config.exs:

          config :popcorn, extra_apps: #{inspect(missing)}

        and recompile the project
        """
    end

    builtin_apps = [:erts, :popcorn_lib | builtin_deps]

    builtin_ebins =
      Enum.flat_map(builtin_apps, fn app -> app |> Build.patched_ebin_dir() |> ls_paths() end)

    dep_ebins =
      Mix.Project.build_path()
      |> Path.join("lib/*/ebin/*")
      |> Path.wildcard()
      |> Enum.reject(fn path ->
        is_popcorn_subpath = Path.relative_to(path, @popcorn_path) != path
        is_popcorn_subpath and not beam_src_in_api_dir?(path)
      end)

    consolidated_ebins = ls_paths(Mix.Project.consolidation_path())
    generated_ebins = ls_paths(@popcorn_generated_path)

    builtin_ebins ++ dep_ebins ++ consolidated_ebins ++ generated_ebins
  end

  defp pack_bundle(out_dir, beams, drop_lines) do
    bundle_path = Path.join(out_dir, "bundle.avm")

    # packbeam appends to the bundle if output exists, we need to delete it first
    maybe_rm(bundle_path)
    bundle_path = to_charlist(bundle_path)
    beams = Enum.map(beams, &String.to_charlist/1)

    :packbeam_api.create(bundle_path, beams, %{
      start_module: Popcorn.Boot,
      include_lines: not drop_lines
    })

    gzip(bundle_path)
  end

  defp maybe_rm(path) do
    case File.rm(path) do
      :ok -> :ok
      {:error, :enoent} -> :ok
      error -> raise CookingError, "Couldn't remove old bundle, reason: #{inspect(error)}"
    end
  end

  @popcorn_app_path Path.join(@popcorn_path, "ebin/popcorn.app")
  defp beam_src_in_api_dir?(@popcorn_app_path), do: false

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

  defp ensure_option_present(options, key, name) do
    if options[key] == nil do
      raise CookingError, """
      #{name} not provided. \
      Please provide the `#{key}` option or configure it by putting \
      `config :popcorn, #{key}: value` in your `config.exs`.
      """
    end
  end

  defp create_appconfig_module(app, start_module, apps_specs) do
    # Ensure shell_history is disabled as it will cause crash due to unimplemented IO & others
    apps_specs = put_in(apps_specs[:kernel][:env][:shell_history], :disabled)

    contents =
      quote location: :keep do
        @compile autoload: false

        def get_config() do
          %{
            app: unquote(app),
            start_module: unquote(start_module),
            apps_specs: unquote(Macro.escape(apps_specs))
          }
        end
      end

    module_name = Popcorn.AppConfig

    {:module, _module_name, binary_content, _term} =
      Module.create(module_name, contents, Macro.Env.location(__ENV__))

    File.mkdir_p!(@popcorn_generated_path)
    path = Path.join(@popcorn_generated_path, "#{module_name}.beam")
    File.write!(path, binary_content)
    path
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

        # TODO: Until separation of deps for tasks and runtime API is solved, disable apps needed by Popcorn's tasks
        spec =
          case app do
            :popcorn ->
              non_api_deps = [
                :inets,
                :ssl,
                :public_key,
                :crypto,
                :asn1,
                :async_test,
                :playwright
              ]

              Keyword.update!(spec, :applications, &(&1 -- non_api_deps))

            _app ->
              spec
          end

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

  defp gzip(path) do
    File.read!(path) |> :zlib.gzip() |> then(&File.write!("#{path}.gz", &1))
  end

  defp ls_paths(path) do
    Path.wildcard(Path.join(path, "*"))
  end
end
