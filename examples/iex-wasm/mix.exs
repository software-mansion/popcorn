defmodule IExWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :iex_wasm,
      version: "0.1.0",
      deps: [{:popcorn, path: "../../popcorn/elixir"}]
    ]
  end
end
