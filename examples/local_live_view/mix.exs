defmodule LocalLiveView.MixProject do
  use Mix.Project

  def project do
    [
      app: :local_live_view,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      aliases: [
        lint: [
          "format --check-formatted",
          "deps.unlock --check-unused",
          "deps.compile",
          "compile --force --warnings-as-errors",
          "docs --warnings-as-errors"
        ]
      ],
      deps: deps(),

      # docs
      name: "LocalLiveView",
      docs: docs()
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
      {:ex_doc, "~> 0.34", only: [:dev, :test], runtime: false, warn_if_outdated: true},
      {:popcorn, path: "../.."},
      {:telemetry, "~> 0.4.3 or ~> 1.0"}
    ]
  end

  defp docs do
    [
      main: "welcome",
      filter_modules: ~r/^(?!Elixir.Phoenix\.).*/,
      extras: [
        "pages/introduction/welcome.md",
        "pages/getting_started/first_steps.md"
      ],
      groups_for_extras: [
        Introduction: ~r"/introduction/",
        "Getting started": ~r"/getting_started/"
      ]
    ]
  end
end
