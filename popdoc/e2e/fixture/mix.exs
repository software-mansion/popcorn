defmodule Example.MixProject do
  use Mix.Project

  def project do
    [
      app: :example,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      docs: &docs/0
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.40", only: :dev, runtime: false},
      {:popdoc, path: "../..", only: :dev, runtime: false}
    ]
  end

  defp docs do
    Popdoc.config(
      main: "readme",
      extras: ["README.md"],
      coi_serviceworker: Mix.env() == :dev
    )
  end
end
