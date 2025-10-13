defmodule DemoModalOffline.MixProject do
  use Mix.Project

  def project do
    [
      app: :demo_modal_offline,
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
      mod: {DemoModalOffline.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:local_live_view, path: "../../../local_live_view"}
    ]
  end
end
