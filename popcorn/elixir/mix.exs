defmodule Popcorn.MixProject do
  use Mix.Project

  @version "0.4.0-next.5"
  @github "https://github.com/software-mansion/popcorn"

  def project do
    [
      app: :popcorn,
      version: @version,
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      description: "Elixir API for Popcorn's OTP/BEAM WebAssembly runtime",
      package: package(),
      aliases: aliases(),
      name: "Popcorn",
      docs: &docs/0,
      source_url: @github,
      homepage_url: "https://popcorn.swmansion.com"
    ]
  end

  def application do
    [extra_applications: [:logger], mod: {Popcorn.Application, []}]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp package do
    [
      maintainers: ["Software Mansion"],
      licenses: ["Apache-2.0"],
      files: ["lib", "patches", "priv/static", "mix.exs", "README.md", "LICENSE"],
      links: %{
        "GitHub" => @github,
        "Popcorn website" => "https://popcorn.swmansion.com"
      }
    ]
  end

  defp aliases do
    [
      "hex.build": &package_with_static("hex.build", &1),
      "hex.publish": &package_with_static("hex.publish", &1)
    ]
  end

  defp package_with_static(task, args) do
    case Enum.any?(args, &(&1 in ["docs", "--revert"])) do
      true -> Mix.Task.run(task, args)
      false -> with_static(fn -> Mix.Task.run(task, args) end)
    end
  end

  defp with_static(fun) do
    out = Path.expand("../out/js", __DIR__)
    static = Path.join(__DIR__, "priv/static")
    stage = Path.join(__DIR__, "_build/hex-static")

    File.rm_rf!(stage)

    try do
      File.mkdir_p!(Path.dirname(stage))
      File.cp_r!(out, stage, dereference_symlinks: true)

      File.mkdir_p!(Path.dirname(static))
      File.rm_rf!(static)
      File.rename!(stage, static)

      try do
        fun.()
      after
        File.rm_rf!(static)
        File.ln_s!("../../out/js", static)
      end
    after
      File.rm_rf!(stage)
    end
  end

  defp docs do
    [
      main: "about",
      markdown_processor: {ExDoc.Markdown.Earmark, footnotes: true},
      extras: [
        "pages/getting-started/about.md",
        "pages/getting-started/installation.md",
        "pages/getting-started/hello-world.md",
        "pages/getting-started/hello-counters.md"
      ],
      groups_for_extras: [
        "Getting started": ~r"/getting-started/",
        Guides: ~r"/guides/",
        Concepts: ~r"/concepts/",
        Contributing: ~r"/contributing/"
      ],
      assets: %{"pages/assets" => "assets"},
      formatters: ["html"],
      before_closing_head_tag: &docs_head/1,
      before_closing_body_tag: &docs_body/1,
      source_url_pattern: "#{@github}/blob/v#{@version}/popcorn/elixir/%{path}#L%{line}",
      source_ref: "v#{@version}"
    ]
    |> ExDocFolds.configure()
    |> ExDocJs.configure(
      entry_points: ["../js/src/index.ts"],
      tsconfig: "../js/tsconfig.json",
      root_module: "JSLib"
    )
  end

  defp docs_head(:html) do
    """
    <style>
      .makeup-string {
        font-style: italic;
      }

      .content-inner .footnotes > ol > li > p {
        margin: 0;
      }

      .content-inner .footnotes > ol > li {
        position: relative;
        padding-right: 1.5em;
      }

      .content-inner .reversefootnote {
        position: absolute;
        right: 0;
        top: 50%;
        transform: translateY(-50%);
      }

      .content-inner a.footnote {
        font-size: 0.75em;
        line-height: 0;
        vertical-align: super;
      }
    </style>
    """
  end

  defp docs_head(_format), do: ""

  defp docs_body(:html) do
    """
    <script>
      function wrapMakeupStrings() {
        document.querySelectorAll("code.makeup").forEach((code) => {
          const starts = code.querySelectorAll('[data-group-id="makeup-string-start"]');
          const ends = code.querySelectorAll('[data-group-id="makeup-string-end"]');

          starts.forEach((start, index) => {
            const end = ends[index];
            if (!end || start.closest(".makeup-string")) return;

            const range = document.createRange();
            const wrapper = document.createElement("span");
            wrapper.className = "makeup-string";
            range.setStartBefore(start);
            range.setEndAfter(end);
            wrapper.append(range.extractContents());
            range.insertNode(wrapper);
          });
        });
      }

      window.addEventListener("exdoc:loaded", wrapMakeupStrings);
      window.addEventListener("exdoc:loaded", () => {
        document.querySelectorAll("a.reversefootnote").forEach((link) => {
          const item = link.closest("li");
          link.textContent = "↵";
          item.append(link);
        });
      });
    </script>
    """
  end

  defp docs_body(_format), do: ""

  defp deps do
    [
      {:brotli, "~> 0.3.3", runtime: false},
      {:file_system, "~> 1.0", runtime: false},
      {:req, "~> 0.5.0 or ~> 0.6.0 or ~> 0.7.0 or == 0.8.0-rc.0", optional: true},
      {:exdoc_folds, path: "../exdoc_folds", only: [:dev, :test], runtime: false},
      {:ex_doc, github: "software-mansion-labs/ex_doc", only: [:dev, :test], runtime: false},
      {:makeup_strings, path: "../makeup_strings", only: :dev, runtime: false},
      {:makeup_syntect, "~> 0.1.4", only: :dev, runtime: false},
      {:makeup_diff, ">= 0.0.0", only: :dev, runtime: false},
      {:makeup_html, ">= 0.0.0", only: :dev, runtime: false},
      {:ex_doc_js, github: "software-mansion-labs/ex_doc_js", only: [:dev, :test], runtime: false}
    ]
  end
end
