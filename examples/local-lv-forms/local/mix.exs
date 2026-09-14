defmodule Local.MixProject do
  use Mix.Project

  def project do
    [
      app: :local,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps_path: "../deps",
      lockfile: "../mix.lock",
      deps: deps(),
      compilers: Mix.compilers(),
      aliases: aliases()
    ]
  end

  def cli do
    [default_target: :wasm]
  end

  def application do
    [extra_applications: [:logger]] ++ app_mod(Mix.target())
  end

  defp app_mod(:host), do: []
  defp app_mod(_), do: [mod: {Local.Application, []}]

  defp aliases do
    [
      build: ["deps.get", "popcorn.cook"],
      dev: ["build", "popcorn.server"]
    ]
  end

  defp deps do
    [
      {:gettext, "~> 1.0"},
      {:local_live_view, path: "../../../local-live-view"}
    ]
  end
end
