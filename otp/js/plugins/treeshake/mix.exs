defmodule PopcornTreeshake.MixProject do
  use Mix.Project

  def project do
    [
      app: :popcorn_treeshake,
      version: "0.1.0",
      elixir: "~> 1.17",
      elixirc_paths: [
        "lib",
        Path.expand("../../../../popcorn/elixir/lib/treeshake", __DIR__)
      ]
    ]
  end
end
