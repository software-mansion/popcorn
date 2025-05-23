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
      strict: [out_dir: :string, target: :string, start_module: :string],
      aliases: [d: :out_dir, m: :start_module]
    ]

    {options, _rest} = OptionParser.parse!(args, parser_config)

    options =
      Enum.map(options, fn
        {:start_module, module} -> {:start_module, Module.concat([module])}
        entry -> entry
      end)

    Popcorn.cook(options)
  end
end
