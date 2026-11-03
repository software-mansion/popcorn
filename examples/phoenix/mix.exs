defmodule PhxWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :phx_wasm,
      version: "0.1.0",
      elixir: "~> 1.20",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {PhxWasm.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"},
      {:phoenix, "~> 1.8"},
      # For Popcorn.Plug's WebSockets
      {:websock_adapter, "~> 0.7.1"},
      {:phoenix_html, "~> 4.2"},
      {:phoenix_live_view, "~> 1.1"},
      {:jason, "~> 1.4"}
    ]
  end

  defp aliases do
    ["popcorn.dev": [&copy_phoenix_js/1, "popcorn.dev"]]
  end

  # The page imports Phoenix's and LiveView's JS as they ship in the deps
  defp copy_phoenix_js(_args) do
    File.mkdir_p!("public/vendor")
    File.cp!("deps/phoenix/priv/static/phoenix.mjs", "public/vendor/phoenix.mjs")

    File.cp!(
      "deps/phoenix_live_view/priv/static/phoenix_live_view.esm.js",
      "public/vendor/phoenix_live_view.esm.js"
    )
  end
end
