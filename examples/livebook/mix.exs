defmodule LivebookWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :livebook_wasm,
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
      mod: {LivebookWasm.Application, []}
    ]
  end

  # Livebook is started by LivebookWasm.Application, once its stand-ins run
  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"},
      {:livebook, "~> 0.19.10", runtime: false},
      # For Popcorn.Plug's WebSockets. Livebook's Phoenix 1.8.7 requires ~> 0.5.3.
      {:websock_adapter, "~> 0.7.1", override: true}
    ]
  end

  defp aliases do
    ["popcorn.dev": [&copy_livebook_static/1, "popcorn.dev"]]
  end

  # The browser loads Livebook's static files from the network, not the VM
  defp copy_livebook_static(_args) do
    for dir <- ~w(assets images favicons iframe) do
      File.cp_r!(Path.join("deps/livebook/priv/static", dir), Path.join("public", dir))
    end
  end
end
