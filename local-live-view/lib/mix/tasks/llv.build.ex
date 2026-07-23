defmodule Mix.Tasks.Llv.Build do
  use Mix.Task

  @shortdoc "Copies LLV runtime assets and builds the WASM bundle"

  @moduledoc """
  Copies LocalLiveView runtime files into the host project and builds the WASM bundle.

  ## Steps

    1. Copies Popcorn runtime files (`iframe.mjs`, `AtomVM.mjs`, `AtomVM.wasm`)
       from `deps/local_live_view/priv/static/` into `priv/static/assets/js/`.
       These must be served alongside Phoenix's `app.js` so Popcorn's
       `import.meta.url`-based lookups resolve.
    2. Runs `mix build` inside `local/` to compile the WASM bundle.

  ## Usage

      mix llv.build

  Add to your `setup` alias in `mix.exs` (done automatically by `mix llv.install`):

      defp aliases do
        [setup: ["deps.get", "llv.build", ...]]
      end
  """

  @runtime_files ~w(iframe.mjs AtomVM.mjs AtomVM.wasm)

  @impl Mix.Task
  def run(args) do
    # `--local` points at the local (WASM) project. Defaults to "local" (a
    # subdirectory of the host app); in an umbrella it's a sibling, e.g.
    # `mix llv.build --local ../local`.
    {opts, _} = OptionParser.parse!(args, strict: [local: :string])
    local_dir = opts[:local] || "local"

    unless File.dir?(local_dir) do
      Mix.raise("#{local_dir}/ directory not found. Run `mix llv.install` first.")
    end

    llv_path = Mix.Project.deps_paths()[:local_live_view]
    src_dir = Path.join(llv_path, "priv/static")
    dst_dir = "priv/static/assets/js"

    Mix.shell().info("[llv] Copying runtime files to #{dst_dir}/")
    File.mkdir_p!(dst_dir)
    File.mkdir_p!(Path.join(dst_dir, "wasm"))

    for file <- @runtime_files do
      File.cp!(Path.join(src_dir, file), Path.join(dst_dir, file))
    end

    Mix.shell().info("[llv] Building WASM bundle...")
    0 = Mix.shell().cmd("mix build", cd: local_dir)
  end
end
