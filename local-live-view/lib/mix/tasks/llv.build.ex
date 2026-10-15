defmodule Mix.Tasks.Llv.Build do
  use Mix.Task

  @shortdoc "Builds LLV's JS bundle and the local project's Wasm runtime"

  @moduledoc """
  Builds LocalLiveView's JS bundle and the Wasm runtime of the local project.

  ## Steps

    1. Makes sure LocalLiveView's JS bundle, which `assets/js/app.js` imports,
       is built.
    2. Fetches the dependencies of the local project and packages it with
       `mix popcorn.cook` into `priv/static/assets/js/popcorn/`, along with
       the Popcorn runtime. LocalLiveView's JS loads it from there: the
       `popcorn/` directory next to the `app.js` bundle. Unused code of the
       dependencies is tree-shaken, while the local project is kept whole.

  ## Usage

      mix llv.build

  It can be run from the local project too, in which case it only packages
  the local project, into the host app's (the parent directory's)
  `priv/static/assets/js/popcorn/`.

  Add to your `setup` alias in `mix.exs` (done automatically by `mix llv.install`):

      defp aliases do
        [setup: ["deps.get", "llv.build", ...]]
      end
  """

  @out_dir "priv/static/assets/js/popcorn"

  # Web assets, served by the host app, and generators' templates
  @excluded_priv ["*/priv/static", "phoenix/priv/templates", "local_live_view/priv"]

  # The target is only :wasm in the local project (it has `default_target: :wasm`)
  @impl Mix.Task
  def run(args) do
    if Mix.target() == :wasm, do: cook_local(), else: build(args)
  end

  defp cook_local() do
    host_dir = ".."

    unless File.regular?(Path.join(host_dir, "mix.exs")) do
      Mix.raise("Host app not found in the parent directory, run `mix llv.build` from there.")
    end

    Mix.Task.run("popcorn.cook", ["--out-dir", Path.expand(@out_dir, host_dir)] ++ cook_args())
  end

  defp build(args) do
    # `--local` points at the local (Wasm) project. Defaults to "local" (a
    # subdirectory of the host app); in an umbrella it's a sibling, e.g.
    # `mix llv.build --local ../local`.
    {opts, _} = OptionParser.parse!(args, strict: [local: :string])
    local_dir = opts[:local] || "local"

    unless File.dir?(local_dir) do
      Mix.raise("#{local_dir}/ directory not found. Run `mix llv.install` first.")
    end

    Mix.Task.run("llv.assets")

    0 = Mix.shell().cmd("mix deps.get", cd: local_dir)

    Mix.shell().info("[llv] Building the Wasm runtime...")

    case cook(local_dir) do
      0 -> :ok
      status -> Mix.raise("[llv] `mix popcorn.cook` failed (#{status})")
    end
  end

  @doc false
  def out_dir, do: @out_dir

  # Packages the local project into `@out_dir`. Returns the exit status.
  defp cook(local_dir) do
    {_output, status} =
      System.cmd(
        "mix",
        ["popcorn.cook", "--out-dir", Path.absname(@out_dir)] ++ cook_args(),
        cd: local_dir,
        into: IO.stream(),
        stderr_to_stdout: true
      )

    status
  end

  @doc false
  # `mix popcorn.cook`'s options for the local project, also used by the e2e
  # tests. Popcorn's tree-shaker knows to keep LocalLiveView's views.
  def cook_args do
    Enum.flat_map(@excluded_priv, &["--exclude-priv", &1]) ++ ["--treeshake"]
  end
end
