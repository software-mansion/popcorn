defmodule Local.MixProject do
  use Mix.Project

  # Builds the local views to the Wasm bundle, see LocalLiveView.E2E.Server.
  # For server-side rendering, they're compiled into local_live_view's test
  # build too.
  def project do
    [
      app: :local,
      version: "0.1.0",
      elixir: "~> 1.17",
      deps_path: "../../../deps",
      lockfile: "../../../mix.lock",
      # Outside test/, which `mix test` searches for test files
      build_path: "../../../_build/e2e_local",
      deps: [{:local_live_view, path: "../../.."}],
      aliases: [build: ["deps.get", "popcorn.cook"]]
    ]
  end

  def cli do
    [default_target: :wasm]
  end

  def application do
    [extra_applications: [:logger]] ++ app_mod(Mix.target())
  end

  # The Wasm runtime runs as long as the application does.
  defp app_mod(:host), do: []
  defp app_mod(_target), do: [mod: {Local.Application, []}]
end
