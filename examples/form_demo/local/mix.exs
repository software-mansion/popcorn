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
      aliases: aliases(),
      preferred_cli_env: [lintformat: :fmt]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Local.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:gettext, "~> 0.26"},
      {:local_live_view, path: "../../local_live_view"},
      {:phoenix, "~> 1.8.0", only: [:fmt], runtime: false},
      {:phoenix_live_view, "~> 1.1.0", only: [:fmt], runtime: false}
    ]
  end

  defp aliases do
    [
      lintformat: "format --check-formatted",
      lint: [
        "deps.unlock --check-unused",
        "deps.compile",
        "compile --force --warnings-as-errors"
      ]
    ]
  end
end
