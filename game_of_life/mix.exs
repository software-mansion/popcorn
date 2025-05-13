defmodule GameOfLife.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_of_life,
      version: "0.1.0",
      elixir: "~> 1.17",
      compilers: Mix.compilers() ++ [:fission_lib],
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build_wasm: "fission_lib.build_avm --target wasm --out-dir static/wasm"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger, :eex],
      mod: {GameOfLife.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:fission_lib, github: "software-mansion-labs/elixir-wasm", sparse: "fission_lib"},
      {:fission_lib, path: "../fission_lib"}
    ]
  end
end
