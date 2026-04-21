defmodule ElixirTour.MixProject do
  use Mix.Project

  def project do
    [
      app: :elixir_tour,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build: ["deps.get", "popcorn.cook"]
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {ElixirTour.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"}
    ]
  end
end
