defmodule MakeupStrings.MixProject do
  use Mix.Project

  def project do
    [
      app: :makeup_strings,
      version: "0.1.0",
      elixir: "~> 1.15",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "Highlights configured Elixir string arguments with Makeup lexers",
      package: [licenses: ["MIT"]],
      docs: [main: "readme", extras: ["README.md"]]
    ]
  end

  def application do
    [
      mod: {MakeupStrings.Application, []},
      env: [languages: []]
    ]
  end

  defp deps do
    [
      {:makeup, "~> 1.2"},
      {:makeup_elixir, "~> 1.0"},
      {:ex_doc, "~> 0.40", only: :dev, runtime: false}
    ]
  end
end
