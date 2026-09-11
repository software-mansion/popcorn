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
      deps: [{:local_live_view, path: "../../../local-live-view"}],
      elixirc_paths: elixirc_paths(Mix.target()),
      compilers: Mix.compilers(),
      aliases: [build: ["deps.get", "popcorn.cook"]]
    ]
  end

  def cli do
    [default_target: :wasm]
  end

  def application do
    [extra_applications: [:logger]] ++ app_mod(Mix.target())
  end

  defp elixirc_paths(:host), do: []
  defp elixirc_paths(_), do: ["lib"]

  defp app_mod(:host), do: []
  defp app_mod(_), do: [mod: {Local.Application, []}]
end
