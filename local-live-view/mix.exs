defmodule LocalLiveView.MixProject do
  use Mix.Project

  @version "0.1.0"
  @popcorn_version "0.3.3"
  @github "https://github.com/software-mansion/popcorn"

  # LICENSE lives in the repo root and is copied in before packaging, see copy_meta/1.
  @repo_root ".."
  @package_metadata_files ~w(LICENSE)

  def project do
    [
      app: :local_live_view,
      version: @version,
      elixir: "~> 1.17",
      elixirc_paths: elixirc_paths(Mix.target()),
      start_permanent: Mix.env() == :prod,
      aliases: aliases(),
      preferred_cli_target: [
        docs: :all,
        "hex.publish": :all
      ],
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
      # priv/static holds the JS bundle and Popcorn runtime files. They are not
      # committed — the release pipeline builds them via `mix llv.assets` (wired
      # into hex.build/hex.publish below) and they ship inside the tarball, so
      # apps get all their JS through Mix with no npm involved.
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
      {:phoenix, "~> 1.8", runtime: false},
      {:phoenix_live_view, "~> 1.1", runtime: false},
      {:phoenix_html, "~> 4.1", runtime: false},
      {:phoenix_ecto, "~> 4.6", runtime: false},
      {:ecto, "~> 3.12", runtime: false},
      {:plug, "~> 1.14", runtime: false},
      {:tailwind, "~> 0.3", runtime: false},
      {:telemetry, "~> 0.4.3 or ~> 1.0"},
      {:file_system, "~> 1.0", targets: [:host, :all]}
    ]
  end

  @popcorn_source Path.expand("../popcorn-2/elixir", __DIR__)

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
        "pages/guides/navigation.md",
        "pages/reference/js-api.md"
      ],
      groups_for_extras: [
        Introduction: ~r"/introduction/",
        "Getting started": ~r"/getting-started/",
        Guides: ~r"/guides/",
        Reference: ~r"/reference/"
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

  defp elixirc_paths(:all), do: ["lib/server", "lib/mix", "lib/local_live_view", "lib/stubs"]
  defp elixirc_paths(:wasm), do: ["lib/local_live_view", "lib/stubs"]
  defp elixirc_paths(_), do: ["lib/server", "lib/mix"]

  defp aliases() do
    [
      lint: [
        "format --check-formatted",
        "deps.unlock --check-unused",
        "deps.compile",
        "compile --force --warnings-as-errors",
        "cmd MIX_TARGET=all mix docs --warnings-as-errors"
      ],
      build: [
        "deps.get",
        "compile",
        &pnpm_install/1,
        fn _args ->
          {_out, 0} =
            System.shell("MIX_TARGET=wasm mix popcorn.cook", into: IO.stream(:stdio, :line))
        end
      ],
      # build_assets/1 guarantees priv/static is built and current, so a tarball
      # can never go out without the JS bundle it is supposed to ship.
      "hex.build": [&copy_meta/1, &build_assets/1, "hex.build"],
      "hex.publish": [&copy_meta/1, &build_assets/1, "hex.publish"]
    ]
  end

  defp pnpm_install(_) do
    {_, 0} =
      System.cmd("pnpm", ["install"],
        cd: File.cwd!(),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
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
