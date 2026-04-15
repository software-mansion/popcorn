defmodule Local.MixProject do
  use Mix.Project

  def project do
    [
      app: :local,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: Mix.compilers(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Local.Application, []}
    ]
  end

  defp deps do
    [
      {:local_live_view, path: "../../../local-live-view"}
    ]
  end

  defp aliases do
    [
      build: ["deps.get", "popcorn.cook"],
      dev: ["build", "popcorn.server"]
    ]
  end
end
