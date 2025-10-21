defmodule LocalLiveView.MixProject do
  use Mix.Project

  def project do
    [
      app: :local_live_view,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {LocalLiveView.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:popcorn, path: "../.."}
#      {:phoenix_html, "~> 4.1"}
    ]
  end
end
