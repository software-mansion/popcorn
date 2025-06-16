defmodule EvalInWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :eval_in_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      compilers: Mix.compilers(),
      deps: deps(),
      aliases: [
        build_wasm: "popcorn.build_runtime --target wasm"
      ]
    ]
  end

  def application do
    [
      extra_applications: [],
      mod: {EvalInWasm.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../"},
      {:playwright, github: "membraneframework-labs/playwright-elixir", only: :test}
    ]
  end
end
