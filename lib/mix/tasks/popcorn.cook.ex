defmodule Mix.Tasks.Popcorn.Cook do
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
