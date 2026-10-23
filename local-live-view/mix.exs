defmodule LocalLiveView.MixProject do
  use Mix.Project

  @version "0.1.0"
  @popcorn_version "0.4.0-next.4"
  @github "https://github.com/software-mansion/popcorn"

  # LICENSE lives in the repo root and is copied in before packaging, see copy_meta/1.
  @repo_root ".."
  @package_metadata_files ~w(LICENSE)

  def project do
    [
      app: :local_live_view,
      version: @version,
      elixir: "~> 1.19",
      elixirc_paths: elixirc_paths(Mix.target()) ++ test_elixirc_paths(Mix.env(), Mix.target()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      deps: deps(),

      # hex
      description: "LiveView that runs locally in the browser, on Popcorn",
      package: package(),

      # docs
      name: "LocalLiveView",
      source_url: @github,
      docs: docs()
    ]
  end

  defp package do
    [
      maintainers: ["Software Mansion"],
      licenses: ["Apache-2.0"],
      # priv/static holds the JS bundle. It is not committed — the release
      # pipeline builds it via `mix llv.assets` (wired into hex.build/hex.publish
      # below) and it ships inside the tarball, so apps get all their JS through
      # Mix with no npm involved. The Wasm runtime comes from the Popcorn
      # package, via `mix llv.build`.
      #
      # package.json must ship too: Phoenix's esbuild config puts deps/ on
      # NODE_PATH, so `import ... from "local_live_view"` resolves through its
      # "exports" entry. assets/ is deliberately left out — users get the built
      # bundle, not the TypeScript sources.
      files: ~w(lib pages priv/static priv/templates package.json mix.exs README.md LICENSE),
      links: %{
        "GitHub" => @github,
        "Popcorn website" => "https://popcorn.swmansion.com"
      }
    ]
  end

  def cli do
    [preferred_targets: [docs: :all, "hex.publish": :all]]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    base = [extra_applications: [:logger]]

    if Mix.target() == :wasm do
      Keyword.put(base, :mod, {LocalLiveView.Application, []})
    else
      Keyword.put(base, :mod, {LocalLiveView.Application.Host, []})
    end
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      popcorn_dep(),
      {:ex_doc, "~> 0.34",
       only: [:dev, :test], targets: [:all], runtime: false, warn_if_outdated: true},
      {:igniter, ">= 0.7.0", runtime: false},
      {:playwright, "~> 1.49.1-alpha.2", runtime: false, only: :test},
      # playwright pins cowlib ~> 2.7.0 which fails to compile on OTP 28
      {:cowlib, "~> 2.13", override: true, runtime: false, only: :test},
      # For the e2e tests' app, see test/e2e/README.md
      {:bandit, "~> 1.5", runtime: false, only: :test},
      {:esbuild, "~> 0.8", runtime: false, only: :test},
      # Starting OTP apps of Phoenix and LiveView is not needed for LLV
      {:phoenix, "~> 1.8.4", runtime: Mix.target() != :wasm},
      {:phoenix_live_view, "~> 1.1", runtime: Mix.target() != :wasm},
      {:phoenix_html, "~> 4.1"},
      {:plug, "~> 1.14", runtime: Mix.target() != :wasm},
      # plug_crypto is needed in runtime for signing LV tokens
      {:plug_crypto, "~> 2.0"},
      {:telemetry, "~> 0.4.3 or ~> 1.0"}
    ] ++ host_deps(Mix.target())
  end

  # For LocalLiveView.Watcher, which runs on the host only. Left out for Wasm,
  # rather than limited with `targets:`, which Mix rejects, as Popcorn depends on
  # file_system on all targets. This way, Wasm builds get it only from Popcorn.
  defp host_deps(:wasm), do: []
  defp host_deps(_target), do: [{:file_system, "~> 1.0"}]

  @popcorn_source Path.expand("../popcorn/elixir", __DIR__)

  defp popcorn_dep do
    if File.dir?(@popcorn_source) and System.get_env("LLV_RELEASE") == nil do
      {:popcorn, path: @popcorn_source, targets: [:wasm, :all]}
    else
      {:popcorn, "~> #{@popcorn_version}", targets: [:wasm, :all]}
    end
  end

  defp docs do
    [
      source_ref: "llv-v#{@version}/local-live-view",
      main: "welcome",
      filter_modules: ~r/^(?!Elixir.Phoenix\.).*/,
      before_closing_body_tag: &before_closing_body_tag/1,
      extras: [
        "pages/introduction/welcome.md",
        "pages/getting-started/installation.md",
        "pages/guides/first-view.md",
        "pages/guides/mirror-sync.md",
        "pages/guides/navigation.md"
      ],
      groups_for_extras: [
        Introduction: ~r"/introduction/",
        Guides: ~r"/guides/"
      ],
      groups_for_modules: [
        "Browser / Wasm (Popcorn)": [
          ~r/^LocalLiveView$/
        ],
        "Server Side": [
          ~r/^LocalLiveView\..*/
        ],
        "Mix Tasks": [
          ~r/^Mix\.Tasks\./
        ]
      ]
    ]
  end

  defp before_closing_body_tag(:html) do
    """
      <script src="https://cdn.jsdelivr.net/npm/mermaid/dist/mermaid.min.js"></script>
      <script>mermaid.initialize({startOnLoad: true})</script>
    """
  end

  defp before_closing_body_tag(_), do: ""

  # lib/shared holds the `LocalLiveView` behaviour itself: views `use` it, and
  # they compile both to Wasm and on the host (where the server renders them).
  defp elixirc_paths(:all),
    do: ["lib/shared", "lib/server", "lib/mix", "lib/local_live_view"]

  # llv.build too, so that it's available in the local project
  defp elixirc_paths(:wasm),
    do: ["lib/shared", "lib/local_live_view", "lib/mix/tasks/llv.build.ex"]

  defp elixirc_paths(_), do: ["lib/shared", "lib/server", "lib/mix"]

  # The app the e2e tests run against, and its local views, which the app
  # renders on the server. See test/e2e/README.md.
  defp test_elixirc_paths(:test, :host), do: ["test/e2e/support", "test/e2e/local/lib"]
  defp test_elixirc_paths(_env, _target), do: []

  defp aliases() do
    [
      lint: [
        "format --check-formatted",
        "deps.unlock --check-unused",
        "deps.compile",
        "compile --force --warnings-as-errors",
        "cmd --shell MIX_TARGET=all mix docs --warnings-as-errors"
      ],
      build: ["deps.get", "compile", "llv.assets"],
      # build_assets/1 guarantees priv/static is built and current, so a tarball
      # can never go out without the JS bundle it is supposed to ship.
      "hex.build": [&copy_meta/1, &build_assets/1, "hex.build"],
      "hex.publish": [&copy_meta/1, &build_assets/1, "hex.publish"]
    ]
  end

  # In a subprocess on purpose: running llv.assets in-process pulls in
  # deps/loadpaths, which knocks the Hex archive off the code path and makes the
  # following hex.build blow up with `Hex.Mix is not available`.
  defp build_assets(_) do
    {_out, 0} = System.shell("mix llv.assets", into: IO.stream(:stdio, :line))
  end

  defp copy_meta(_) do
    repo_root = Path.expand(@repo_root, __DIR__)

    for filename <- @package_metadata_files do
      File.cp!(Path.join(repo_root, filename), Path.expand(filename, __DIR__))
    end
  end
end
