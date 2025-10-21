defmodule CompareLiveViewsLocal.MixProject do
  use Mix.Project

  def project do
    [
      app: :compare_live_views_local,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      compilers: Mix.compilers() ++ [:local_live_view]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {CompareLiveViewsLocal.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:local_live_view, path: "../../../local_live_view"}
    ]
  end
end
