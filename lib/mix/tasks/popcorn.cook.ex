defmodule Mix.Tasks.Popcorn.Cook do
  @shortdoc "Generates static artifacts to run the project in the browser."
  @moduledoc """
  #{@shortdoc}

  Accepts the following options:
  - `out_dir` - the directory to write artifacts to
  - `target` - `wasm` (default) or `unix`. If `unix` is chosed, you need to build the runtime
  first with `mix popcorn.build_runtime --target unix`

  `out_dir` is mandatory, unless provided via `config.exs`,
  for example `config :popcorn, out_dir: "static/wasm"`
  """
  use Mix.Task

  @requirements "compile"

  @impl true
  def run(args) do
    parser_config = [
      strict: [out_dir: :string, target: :string],
      aliases: [d: :out_dir]
    ]

    {options, _rest} = OptionParser.parse!(args, parser_config)

    {module, filename} = create_start_module()
    options = Keyword.put(options, :start_module, module)

    Popcorn.cook(options)
    File.rm!(filename)
  end

  # module is expected to implement Application behaviour
  defp create_start_module() do
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

      # Intentinally omit line number and whole stacktrace plus ensure file is relative to popcorn root
      IO.warn(
        "Disabled unsupported :logger application, some calls may break at runtime",
        file: Path.relative_to(env.file, Path.expand(__DIR__ <> "../../..")),
        module: __MODULE__,
        function: env.function
      )
    end

    # TODO: Until separation of deps for tasks and runtime is solved, disable all apps from :popcorn
    specs =
      if Map.has_key?(specs, :popcorn) do
        put_in(specs[:popcorn][:applications], [])
      else
        specs
      end

    # Ensure shell_history is disabled as it will cause crash due to unimplemented IO & others
    specs = put_in(specs[:kernel][:env][:shell_history], :disabled)

    # The module below tries to mimic BEAMs boot script, then start user's app
    contents =
      quote do
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

    module_name = Popcorn.Entrypoint

    {:module, _module_name, binary_content, _term} =
      Module.create(module_name, contents, Macro.Env.location(__ENV__))

    filename = Mix.Project.compile_path() |> Path.join("#{module_name}.beam")
    File.write!(filename, binary_content)
    {module_name, filename}
  end

  defp gather_app_specs([], specs), do: specs

  defp gather_app_specs(apps, specs) do
    new_apps = Enum.reject(apps, &Map.has_key?(specs, &1))

    new_specs =
      for app <- new_apps,
          spec = Application.spec(app),
          # Ignore missing optional apps
          spec != nil,
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
end
