defmodule IexWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :iex_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      compilers: Mix.compilers() ++ [:fission_lib],
      deps: deps(),
      aliases: [
        build_wasm: "fission_lib.build_avm --target wasm --out-dir static/wasm"
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
      {:fission_lib, path: "../fission_lib"},
      {:playwright, github: "membraneframework-labs/playwright-elixir", only: :test}
    ]
  end
end
