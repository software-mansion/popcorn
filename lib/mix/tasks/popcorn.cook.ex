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

    app_entrypoint = get_starting_point()
    {module, filename} = create_start_module(app_entrypoint)
    options = Keyword.put(options, :start_module, entrypoint)

    Popcorn.cook(options)
    File.rm!(filename)
  end

  defp get_app_entrypoint() do
    config = Mix.Project.config()
    app = Keyword.get(config, :app)

    case Application.spec(app, :mod) do
      {_mod, _args} = app_mod ->
        app_mod

      _ ->
        raise "Missing application starting point. Please provide `:mod` in application config of your `mix.exs`"
    end
  end

  # module is expected to implement Application behaviour
  defp create_start_module({module, args}) do
    contents =
      quote do
        def start() do
          unquote(module).start(:normal, unquote(args))
        end
      end

    module_name = Popcorn.Entrypoint

    {:module, _module_name, binary_content, _term} =
      Module.create(module_name, contents, Macro.Env.location(__ENV__))

    filename = Mix.Project.compile_path() |> Path.join("#{module_name}.beam")
    File.write!(filename, binary_content)
    {module_name, filename}
  end
end
