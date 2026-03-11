defmodule LocalThermostat.MixProject do
  use Mix.Project

  def project do
    [
      app: :local_thermostat,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build: ["deps.get", "popcorn.cook"],
        dev: ["build", "popcorn.server --dir static"]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {LocalThermostat.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:local_live_view, path: "../../local-live-view"}
    ]
  end
end
