defmodule HelloPopcorn.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello_popcorn,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build_wasm: ["popcorn.build_runtime --target wasm", "popcorn.cook"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [],
      mod: {HelloPopcorn.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:popcorn, github: "software-mansion/popcorn"}
      {:popcorn, "~> 0.1.0"},
      # playwright will be started manually
      {:playwright,
       github: "membraneframework-labs/playwright-elixir", runtime: false, only: :test}
    ]
  end
end
