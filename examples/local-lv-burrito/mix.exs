defmodule Burrito.MixProject do
  use Mix.Project

  def project do
    [
      app: :burrito,
      version: "0.1.0",
      elixir: "~> 1.15",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      releases: releases(),
      deps: deps(),
      compilers: [:phoenix_live_view] ++ Mix.compilers(),
      listeners: [Phoenix.CodeReloader]
    ]
  end

  def application do
    [
      mod: {Burrito.Application, []},
      extra_applications: [:logger, :runtime_tools]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  defp deps do
    [
      {:phoenix, "~> 1.8.0"},
      {:phoenix_html, "~> 4.1"},
      {:phoenix_live_reload, "~> 1.2", only: :dev},
      {:phoenix_live_view, "~> 1.1.0"},
      {:phoenix_live_dashboard, "~> 0.8.3"},
      {:tailwind, "~> 0.3", runtime: Mix.env() == :dev},
      {:esbuild, "~> 0.10", runtime: Mix.env() == :dev},
      {:heroicons,
       github: "tailwindlabs/heroicons",
       tag: "v2.2.0",
       sparse: "optimized",
       app: false,
       compile: false,
       depth: 1},
      {:swoosh, "~> 1.16"},
      {:req, "~> 0.5"},
      {:telemetry_metrics, "~> 1.0"},
      {:telemetry_poller, "~> 1.0"},
      {:gettext, "~> 0.26"},
      {:jason, "~> 1.2"},
      {:dns_cluster, "~> 0.2.0"},
      {:bandit, "~> 1.5"},
      {:local_live_view,
       path: System.get_env("LOCAL_LIVE_VIEW_PATH", "../../local-live-view"), runtime: false},
      {:local, path: "local"}
    ]
  end

  defp releases do
    [
      burrito: [
        applications: [local_live_view: :load]
      ]
    ]
  end

  defp aliases do
    [
      build: ["setup"],
      dev: ["setup", "phx.server"],
      setup: ["deps.get", "llv.build", "assets.setup", "assets.build"],
      "assets.setup": ["tailwind.install --if-missing", "esbuild.install --if-missing"],
      "assets.build": ["tailwind burrito", "esbuild burrito"],
      "assets.deploy": [
        "tailwind burrito --minify",
        "esbuild burrito --minify",
        "phx.digest"
      ],
      precommit: ["compile --warning-as-errors", "deps.unlock --unused", "format", "test"]
    ]
  end
end
