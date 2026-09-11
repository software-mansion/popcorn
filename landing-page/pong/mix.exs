defmodule LandingPong.MixProject do
  use Mix.Project

  def project do
    [
      app: :landing_pong,
      version: "0.1.0",
      elixir: "~> 1.17",
      deps: [
        {:local_live_view, path: "../../local-live-view"},
        {:local, path: "local"},
        {:phoenix_live_view, "== 1.1.30"}
      ]
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end
end
