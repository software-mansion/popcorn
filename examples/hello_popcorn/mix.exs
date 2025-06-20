defmodule HelloPopcorn.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello_popcorn,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build_wasm: ["popcorn.build_runtime --target wasm", &copy_artefacts/1]
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [],
      mod: {HelloPopcorn.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      # {:popcorn, github: "software-mansion/popcorn"}
      {:popcorn, path: "../.."},
      # playwright will be started manually
      {:playwright, github: "membraneframework-labs/playwright-elixir", runtime: false, only: :test}
    ]
  end

  defp copy_artefacts(_) do
    mjs = Application.app_dir(:popcorn, ["atomvm_artifacts", "wasm", "AtomVM.mjs"])
    wasm = Application.app_dir(:popcorn, ["atomvm_artifacts", "wasm", "AtomVM.wasm"])
    popcorn_js = Application.app_dir(:popcorn, ["priv", "static-template", "wasm", "popcorn.js"])
    popcorn_iframe_js = Application.app_dir(:popcorn, ["priv", "static-template", "wasm", "popcorn_iframe.js"])
    File.cp!(mjs, "static/wasm/AtomVM.mjs")
    File.cp!(wasm, "static/wasm/AtomVM.wasm")
    File.cp!(popcorn_js, "static/wasm/popcorn.js")
    File.cp!(popcorn_iframe_js, "static/wasm/popcorn_iframe.js")
  end
end
