defmodule PopdocWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :popdoc_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: false,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {Popdoc.Wasm.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"}
    ]
  end
end
