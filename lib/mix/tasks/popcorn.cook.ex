defmodule Mix.Tasks.Popcorn.Cook do
  @shortdoc "Generates static artifacts to run the project in the browser."
  @moduledoc """
  #{@shortdoc}

  Accepts the following options:
  - `out_dir` - the directory to write artifacts to
  - `start_module` - a module with `start/0` function, the application's entry point
  - `target` - `wasm` (default) or `unix`. If `unix` is chosed, you need to build the runtime
  first with `mix popcorn.build_runtime --target unix`

  `out_dir` and `start_module` are mandatory, unless provided
  via `config.exs`, for example `config :popcorn, out_dir: "static/wasm, start_module: Start"`
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
    specs = gather_app_specs([:kernel, :stdlib, app], %{})

    # The module below tries to mimic BEAMs boot script, then start user's app
    contents =
      quote do
        def start() do
          # TODO: Default boot script starts `:heart` process
          {:ok, _pid} = :logger_server.start_link()

          specs = unquote(Macro.escape(specs))

          {:ok, _ac} =
            :application_controller.start({:application, :kernel, specs[:kernel]})

          for {app, spec} <- specs, app != :kernel do
            :ok = :application.load({:application, app, spec})
          end

          :ok = :application.start_boot(:kernel, :permanent)
          :ok = :application.start_boot(:stdlib, :permanent)

          {:ok, _apps} = Application.ensure_all_started(unquote(app), :permanent)
          :ok
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
      Map.new(new_apps, fn app ->
        # Application.spec/1 doesn't return env, fetching it manually
        env = Application.get_all_env(app)
        spec = app |> Application.spec() |> List.wrap()
        {app, [env: env] ++ spec}
      end)

    deps =
      new_specs
      |> Enum.flat_map(fn {_app, spec} -> spec[:applications] end)
      |> Enum.uniq()

    gather_app_specs(deps, Map.merge(specs, new_specs))
  end
end
