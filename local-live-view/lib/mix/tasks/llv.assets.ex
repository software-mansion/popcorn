defmodule Mix.Tasks.Llv.Assets do
  use Mix.Task

  @shortdoc "Ensures LocalLiveView's prebuilt JS assets are available"

  @moduledoc """
  Makes sure `local_live_view/priv/static/` holds an up-to-date JS bundle.

  This is the single place that knows where the bundle comes from, and it
  answers differently depending on how `:local_live_view` was fetched:

    * **published package** (Hex tarball, no `assets/` directory) — the bundle
      was built by the release pipeline and ships inside the tarball, so there
      is nothing to do. Nothing is ever built on a user's machine.

    * **source checkout** (path dep, git dep, this repo) — `priv/static/` is
      not committed, so it is built with `pnpm` on demand. The hash of the TS
      sources is cached in `priv/.assets_hash`, so repeated runs are a no-op
      and only actual source changes trigger a rebuild.

  `mix llv.build` runs this first, and every LocalLiveView app has `llv.build`
  in its `setup` alias — so this normally never needs to be invoked directly.

  ## Usage

      mix llv.assets
  """

  @artifacts ~w(local_live_view.js local_live_view.d.ts AtomVM.mjs AtomVM.wasm iframe.mjs)
  @extra_sources ~w(rollup.config.mjs package.json tsconfig.json)
  # Kept outside priv/static so it stays out of the hex package.
  @hash_file "priv/.assets_hash"

  @impl Mix.Task
  def run(_args) do
    llv_dir = llv_dir()
    static_dir = Path.join(llv_dir, "priv/static")

    cond do
      not File.dir?(Path.join(llv_dir, "assets")) ->
        verify_prebuilt!(static_dir)

      fresh?(llv_dir, static_dir) ->
        :ok

      true ->
        build!(llv_dir, static_dir)
    end
  end

  # LocalLiveView's own directory: the dep path when we run from a host app,
  # the cwd when we run inside local_live_view itself (its own tests, `mix build`).
  defp llv_dir do
    case Mix.Project.deps_paths()[:local_live_view] do
      nil ->
        if Mix.Project.config()[:app] == :local_live_view do
          File.cwd!()
        else
          Mix.raise(
            ":local_live_view is not a dependency of this project. " <>
              "Add it to your deps in mix.exs."
          )
        end

      path ->
        path
    end
  end

  # Published package: no sources to build from, so a missing artifact means a
  # broken release rather than something the user can fix.
  defp verify_prebuilt!(static_dir) do
    case missing(static_dir) do
      [] ->
        :ok

      missing ->
        Mix.raise("""
        The :local_live_view package is missing prebuilt JS assets: #{Enum.join(missing, ", ")}.

        This is a packaging bug — please report it at
        https://github.com/software-mansion/popcorn/issues
        """)
    end
  end

  defp build!(llv_dir, static_dir) do
    unless System.find_executable("pnpm") do
      Mix.raise("""
      LocalLiveView's JS bundle is missing and cannot be built: pnpm was not found.

      This happens when :local_live_view is used from source (a path or git dep),
      where priv/static/ is not committed. Either install Node.js + pnpm and run

          pnpm install && pnpm run build   # in #{llv_dir}

      or depend on the published package instead: {:local_live_view, "~> 0.1"}.
      """)
    end

    Mix.shell().info("[llv] Installing JS dependencies...")
    cmd!("pnpm", ["install"], llv_dir)

    check_popcorn_dist!(llv_dir)

    Mix.shell().info("[llv] Building JS bundle...")
    cmd!("pnpm", ["run", "build"], llv_dir)

    case missing(static_dir) do
      [] -> File.write!(Path.join(llv_dir, @hash_file), sources_hash(llv_dir))
      missing -> Mix.raise("[llv] JS build did not produce: #{Enum.join(missing, ", ")}")
    end
  end

  # The bundle inlines @swmansion/popcorn and copies the runtime files out of
  # its dist/. In this repo that package is a workspace link to popcorn/js,
  # whose dist/ is generated as well — so check it here instead of letting
  # rollup fail on a bare ENOENT deep inside the plugin.
  defp check_popcorn_dist!(llv_dir) do
    dist = Path.join(llv_dir, "node_modules/@swmansion/popcorn/dist")

    unless File.regular?(Path.join(dist, "AtomVM.wasm")) do
      Mix.raise("""
      #{dist} has no AtomVM.wasm.

      Popcorn's JS package has not been built. From the repo root run:

          pnpm -F popcorn build

      It builds AtomVM, so it needs emsdk (see mise.toml).
      """)
    end
  end

  defp cmd!(command, args, cd) do
    case System.cmd(command, args, cd: cd, into: IO.stream(:stdio, :line), stderr_to_stdout: true) do
      {_output, 0} ->
        :ok

      {_output, status} ->
        Mix.raise("[llv] `#{command} #{Enum.join(args, " ")}` failed (#{status})")
    end
  end

  defp fresh?(llv_dir, static_dir) do
    missing(static_dir) == [] and
      File.read(Path.join(llv_dir, @hash_file)) == {:ok, sources_hash(llv_dir)}
  end

  defp missing(static_dir) do
    Enum.reject(@artifacts, &File.regular?(Path.join(static_dir, &1)))
  end

  defp sources_hash(llv_dir) do
    sources =
      Path.wildcard(Path.join(llv_dir, "assets/local_live_view/**/*.ts")) ++
        Enum.map(@extra_sources, &Path.join(llv_dir, &1))

    sources
    |> Enum.sort()
    |> Enum.map(&{Path.relative_to(&1, llv_dir), File.read(&1)})
    |> :erlang.term_to_binary()
    |> then(&Base.encode16(:crypto.hash(:sha256, &1), case: :lower))
  end
end
