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

    Popcorn.cook(options)
  end
end
