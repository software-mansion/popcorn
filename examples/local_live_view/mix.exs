defmodule LocalLiveView.MixProject do
  use Mix.Project

  def project do
    [
      app: :local_live_view,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
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
        "README.md"
      ],
      groups_for_extras: [
        Introduction: ~r"/introduction/",
        "Getting started": "README.md"
      ]
    ]
  end

  @static_dir "static/local_live_view"
  @out_dir Application.compile_env(:local_live_view, :out_dir, @static_dir)
  
  defp aliases() do
    [
      lint: [
        "format --check-formatted",
        "deps.unlock --check-unused",
        "deps.compile",
        "compile --force --warnings-as-errors",
        "docs --warnings-as-errors"
      ],
      compile: ["compile", &copy_js/1]
    ]
  end

  defp copy_js(_) do
    Mix.shell().cmd(
      """
      mkdir -p ../../#{@out_dir}
      cp lib/local_live_view/priv/static/local_live_view.js ../../#{@out_dir}
      """,
      cd: Mix.Project.build_path()
    )
  end
end
