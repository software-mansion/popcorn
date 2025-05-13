defmodule EvalInWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :eval_in_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      compilers: Mix.compilers() ++ [:popcorn],
      deps: deps(),
      aliases: [
        build_wasm: "popcorn.build_avm --target wasm --out-dir static/wasm"
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../"},
      {:playwright, github: "membraneframework-labs/playwright-elixir", only: :test}
    ]
  end
end
