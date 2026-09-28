defmodule LlvIntegration.MixProject do
  use Mix.Project

  # A Phoenix app exercising LocalLiveView end to end in a browser. Not part of
  # the local_live_view package: see README.md.
  def project do
    [
      app: :llv_integration,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: false,
      aliases: aliases(),
      deps: deps(),
      compilers: [:phoenix_live_view] ++ Mix.compilers()
    ]
  end

  def application do
    [
      mod: {LlvIntegration.Application, []},
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:local_live_view, path: ".."},
      {:local, path: "local"},
      {:phoenix, "~> 1.8.5"},
      {:phoenix_html, "~> 4.1"},
      {:phoenix_live_view, "~> 1.1.0"},
      {:bandit, "~> 1.5"},
      {:jason, "~> 1.2"},
      {:esbuild, "~> 0.10"}
    ]
  end

  defp aliases do
    [
      setup: ["deps.get", "esbuild.install --if-missing", "assets.build"],
      # The Wasm bundle (llv.build) and app.js must reflect the current
      # local_live_view sources, so they're rebuilt on every run.
      "assets.build": ["llv.build", "esbuild llv_integration"],
      test: ["assets.build", "test"]
    ]
  end
end
