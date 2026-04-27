defmodule LocalLiveView.MixProject do
  use Mix.Project

  def project do
    [
      app: :local_live_view,
      version: "0.1.0",
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.target()),
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
      {:popcorn, path: "../popcorn/elixir"},
      {:phoenix, "~> 1.8", runtime: false},
      {:phoenix_live_view, runtime: false},
      {:phoenix_html, "~> 4.1", runtime: false},
      {:phoenix_ecto, "~> 4.6", runtime: false},
      {:ecto, "~> 3.12", runtime: false},
      {:plug, "~> 1.14", runtime: false},
      {:tailwind, "~> 0.3", runtime: false},
      {:telemetry, "~> 0.4.3 or ~> 1.0"}
    ]
  end

  defp docs do
    [
      main: "welcome",
      filter_modules: ~r/^(?!Elixir.Phoenix\.).*/,
      extras: [
        "pages/introduction/welcome.md",
        "pages/getting-started/installation.md",
        "pages/guides/first-view.md",
        "pages/guides/mirror-sync.md"
      ],
      groups_for_extras: [
        Introduction: ~r"/introduction/",
        "Getting started": ~r"/getting-started/",
        Guides: ~r"/guides/"
      ]
    ]
  end

  defp elixirc_paths(:wasm), do: ["lib/local_live_view", "lib/stubs"]
  defp elixirc_paths(_), do: ["lib/server"]

  defp aliases() do
    [
      lint: [
        "format --check-formatted",
        "deps.unlock --check-unused",
        "deps.compile",
        "compile --force --warnings-as-errors",
        "docs --warnings-as-errors"
      ],
      build: ["deps.get", &pnpm_install/1, "popcorn.cook"]
    ]
  end

  defp pnpm_install(_) do
    {_, 0} =
      System.cmd("pnpm", ["install"],
        cd: File.cwd!(),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
  end
end
