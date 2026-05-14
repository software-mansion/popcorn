defmodule Mix.Tasks.Llv.Build do
  use Mix.Task

  @shortdoc "Builds LLV JS assets and WASM bundle"

  @moduledoc """
  Builds LocalLiveView JS assets and the WASM bundle.

  ## Steps

    1. Installs `@swmansion/popcorn` and `esbuild` into the library's assets dir
    2. Bundles `local_live_view.js` into `assets/vendor/local_live_view.js`
    3. Copies Popcorn runtime files (`iframe.mjs`, `AtomVM.mjs`, `AtomVM.wasm`)
       into `priv/static/assets/`
    4. Runs `mix build` inside `local/` to compile the WASM bundle

  ## Usage

      mix llv.build

  Add to your `setup` alias in `mix.exs` (done automatically by `mix llv.install`):

      defp aliases do
        [setup: ["deps.get", "llv.build", ...]]
      end
  """

  @popcorn_npm_version "0.3.0-rc2"

  @impl Mix.Task
  def run(_args) do
    unless File.dir?("local") do
      Mix.raise("local/ directory not found. Run `mix llv.install` first.")
    end

    llv_path = Mix.Project.deps_paths()[:local_live_view]
    assets_path = Path.join(llv_path, "assets")

    Mix.shell().info("[llv] Installing JS dependencies...")
    0 = Mix.shell().cmd("npm install --prefix \"#{assets_path}\" @swmansion/popcorn@#{@popcorn_npm_version} esbuild")

    Mix.shell().info("[llv] Building JS assets...")
    0 = Mix.shell().cmd("node \"#{Path.join(assets_path, "bundle.mjs")}\"", env: [{"LLV_PROJECT_DIR", File.cwd!()}])

    Mix.shell().info("[llv] Building WASM bundle...")
    Mix.shell().cmd("mix build", cd: "local")
  end
end
