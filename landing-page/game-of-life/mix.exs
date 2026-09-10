defmodule LandingGameOfLife.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_of_life,
      version: "0.1.0",
      elixir: "~> 1.17",
      deps: [{:popcorn, path: "../../popcorn/elixir"}]
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {GameOfLife.Application, []}
    ]
  end
end
