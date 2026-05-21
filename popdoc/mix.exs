defmodule Popdoc.MixProject do
  use Mix.Project

  @version "0.1.0"
  @source_url "https://github.com/software-mansion/popcorn/popdoc"

  def project do
    [
      app: :popdoc,
      version: @version,
      elixir: "~> 1.17",
      start_permanent: false,
      name: "Popdoc",
      description: "ExDoc extension for interactive Elixir code evaluation, using Popcorn.",
      source_url: @source_url,
      package: package(),
      deps: deps()
    ]
  end

  def application do
    []
  end

  defp package do
    [
      name: "popdoc",
      files: ~w(lib assets docs_assets mix.exs README.md LICENSE.md),
      licenses: ["Apache-2.0"],
      links: %{"Github" => @source_url}
    ]
  end

  defp deps do
    [
      {:ex_doc, ">= 0.34.0 and < 1.0.0", optional: true, runtime: false, warn_if_outdated: true}
    ]
  end
end
