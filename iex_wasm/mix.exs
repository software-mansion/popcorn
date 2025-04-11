defmodule IexWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :iex_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      compilers: Mix.compilers() ++ [:fission_lib],
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:fission_lib, path: "../fission_lib"},
      {:playwright,
       github: "membraneframework-labs/playwright-elixir",
       ref: "5c02249512fa543f5e619a69b7e5c9e046605fe5",
       only: :test},
      {:bandit, "~> 1.1", only: :test}
    ]
  end
end
