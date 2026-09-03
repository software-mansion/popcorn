defmodule Popcorn.MixProject do
  use Mix.Project

  @version "0.1.0"
  @github "https://github.com/software-mansion/popcorn"

  def project do
    [
      app: :popcorn,
      version: @version,
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      description: "Elixir API for Popcorn's OTP/BEAM WebAssembly runtime",
      package: package(),
      name: "Popcorn OTP",
      docs: &docs/0,
      source_url: @github,
      homepage_url: "https://popcorn.swmansion.com"
    ]
  end

  def application do
    [extra_applications: [:logger], mod: {Popcorn.Application, []}]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp package do
    [
      maintainers: ["Software Mansion"],
      licenses: ["Apache-2.0"],
      files: ["lib", "mix.exs", "README.md"],
      links: %{
        "GitHub" => @github,
        "Popcorn website" => "https://popcorn.swmansion.com"
      }
    ]
  end

  defp docs do
    [
      main: "Popcorn.Wasm",
      formatters: ["html"],
      source_ref: "v#{@version}"
    ]
    |> ExDocJs.configure(
      entry_points: ["../js/src/index.ts"],
      tsconfig: "../js/tsconfig.json",
      root_module: "JS"
    )
  end

  defp deps do
    [
      {:req, ">= 0.5.0", optional: true},
      {:ex_doc, github: "software-mansion-labs/ex_doc", only: [:dev, :test], runtime: false},
      {:ex_doc_js, github: "software-mansion-labs/ex_doc_js", only: [:dev, :test], runtime: false}
    ]
  end
end
