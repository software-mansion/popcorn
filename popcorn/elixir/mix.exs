defmodule Popcorn.MixProject do
  use Mix.Project

  @version "0.3.0-rc1"
  @github "https://github.com/software-mansion/popcorn"

  def project do
    otp_version =
      "#{:code.root_dir()}/releases/#{System.otp_release()}/OTP_VERSION"
      |> File.read!()
      |> String.trim()

    unless otp_version == "26.0.2" do
      raise "Popcorn only supports OTP 26.0.2 and Elixir 1.17.3"
    end

    [
      app: :popcorn,
      version: @version,
      elixir: "1.17.3",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      aliases: [
        docs: ["docs", &generate_js_docs/1],
        compile: ["compile", &patch/1],
        lint: [
          "format --check-formatted",
          "deps.unlock --check-unused",
          "credo",
          "deps.compile",
          "compile --force --warnings-as-errors",
          "docs --warnings-as-errors"
          # FIXME: fix/ignore playwright's messed up specs and re-enable dialyzer
          # "dialyzer"
        ]
      ],
      dialyzer: [plt_add_apps: [:mix, :ex_unit]],
      deps: deps(),

      # hex
      description: "Popcorn: run Elixir in browser",
      package: package(),

      # docs
      name: "Popcorn",
      docs: docs(),
      source_url: @github,
      homepage_url: "https://popcorn.swmansion.com"
    ]
  end

  def application do
    [extra_applications: [:logger, :inets, :ssl, :public_key, :crypto]]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp package do
    [
      maintainers: ["Software Mansion"],
      licenses: ["Apache-2.0"],
      files: ["lib", "patches", "mix.exs", "README.md", "LICENSE", "src"],
      links: %{
        "GitHub" => @github,
        "Popcorn website" => "https://popcorn.swmansion.com"
      }
    ]
  end

  defp docs do
    [
      main: "introduction",
      favicon: "../../assets/favicon.png",
      logo: "../../assets/logo.svg",
      extras: [
        "pages/getting_started/introduction.md",
        "pages/getting_started/first_steps.md",
        "pages/reference/architecture.md",
        "pages/reference/limitations.md",
        "JS Documentation": [url: "js-api/index.html"]
      ],
      groups_for_extras: [
        "Getting started": ~r"/getting_started/",
        Reference: ~r"(?:/reference/|js-api/)"
      ],
      groups_for_modules: [
        "WebAssembly API": [Popcorn.Wasm, Popcorn.TrackedObject]
      ],
      formatters: ["html"],
      before_closing_head_tag: &before_closing_head_tag/1,
      source_ref: "v#{@version}"
    ]
  end

  # Enable mermaid.js support in docs
  defp before_closing_head_tag(:html) do
    """
    <script defer src="https://cdn.jsdelivr.net/npm/mermaid@11.10.1/dist/mermaid.min.js"></script>
    <script>
      let initialized = false;

      window.addEventListener("exdoc:loaded", () => {
        if (!initialized) {
          mermaid.initialize({
            startOnLoad: false,
            theme: document.body.className.includes("dark") ? "dark" : "default"
          });
          initialized = true;
        }

        let id = 0;
        for (const codeEl of document.querySelectorAll("pre code.mermaid")) {
          const preEl = codeEl.parentElement;
          const graphDefinition = codeEl.textContent;
          const graphEl = document.createElement("div");
          const graphId = "mermaid-graph-" + id++;
          mermaid.render(graphId, graphDefinition).then(({svg, bindFunctions}) => {
            graphEl.innerHTML = svg;
            bindFunctions?.(graphEl);
            preEl.insertAdjacentElement("afterend", graphEl);
            preEl.remove();
          });
        }
      });
    </script>
    """
  end

  defp generate_js_docs(_) do
    js_dir = Path.expand("../js", __DIR__)
    js_entry = Path.join(js_dir, "dist/popcorn.mjs")

    run_cmd!("pnpm run build", cd: js_dir)
    run_cmd!("npx documentation build #{js_entry} -f html -o doc/js-api", [])
  end

  defp run_cmd!(command, opts) do
    case Mix.shell().cmd(command, opts) do
      0 -> :ok
      status -> Mix.raise("command failed with status #{status}: #{command}")
    end
  end

  defp patch(_args) do
    Popcorn.Build.build()
  end

  defp deps do
    [
      # {:atomvm_packbeam, github: "atomvm/atomvm_packbeam"},
      {:jason, "~> 1.4"},
      {:treeshake, path: "/Users/matheksm/treeshake", runtime: false},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false},
      # Docs
      {:ex_doc, "~> 0.34", only: [:dev, :test], runtime: false, warn_if_outdated: true},
      {:makeup_html, ">= 0.0.0", only: :dev, runtime: false, warn_if_outdated: true},
      {:makeup_syntect, ">= 0.0.0", only: :dev, runtime: false, warn_if_outdated: true},
      # Testing
      {:async_test, github: "software-mansion-labs/elixir_async_test", only: :test},
      {:playwright, github: "membraneframework-labs/playwright-elixir", only: :test}
    ]
  end
end
