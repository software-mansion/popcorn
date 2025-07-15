defmodule IexWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :iex_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      compilers: Mix.compilers(),
      deps: deps(),
      aliases: [
        build_wasm: "popcorn.build_runtime --target wasm"
      ]
    ]
  end

  def application do
    [
      extra_applications: [:iex],
      mod: {IexWasm.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../.."},
      {:extty, "~> 0.2"}
    ]
  end
end
