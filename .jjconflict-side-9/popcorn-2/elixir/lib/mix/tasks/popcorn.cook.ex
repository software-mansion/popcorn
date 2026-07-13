defmodule Mix.Tasks.Popcorn.Cook do
  @shortdoc "Builds the `.avm` bundle for a Popcorn project."
  @moduledoc """
  #{@shortdoc}

  Accepts the following options:
  - `out_dir` - the directory to write artifacts to
  - `start_module` - optional module with `start/0` used as the bundle entrypoint

  `out_dir` is mandatory, unless provided via `config.exs`,
  for example `config :popcorn, out_dir: "dist/wasm"`
  """
  use Mix.Task

  @requirements "compile"
  @parser_config [
    strict: [out_dir: :string, start_module: :string],
    aliases: [d: :out_dir]
  ]

  @impl true
  def run(args) do
    {options, _rest} = OptionParser.parse!(args, @parser_config)

    options
    |> Keyword.update(:start_module, nil, &as_module/1)
    |> Popcorn.cook()
  end

  defp as_module(module_name) when is_binary(module_name) do
    module_name
    |> String.split(".")
    |> Module.safe_concat()
  end
end
