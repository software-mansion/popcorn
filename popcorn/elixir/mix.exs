defmodule Popcorn.MixProject do
  use Mix.Project

  @version "0.4.0-next.0"
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
      files: ["lib", "priv/static", "mix.exs", "README.md", "LICENSE"],
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
      main: "introduction",
      extras: [
        "pages/getting-started/introduction.md",
        "pages/getting-started/installation.md",
        "pages/getting-started/first-application.md",
        "pages/concepts/runtime-model.md",
        "pages/concepts/values.md",
        "pages/guides/messaging.md",
        "pages/guides/javascript-interop.md",
        "pages/guides/http.md",
        "pages/guides/terminal.md",
        "pages/guides/packaging.md",
        "pages/guides/deployment.md",
        "pages/reference/compatibility.md",
        "pages/reference/troubleshooting.md",
        "pages/migration/from-0-3.md",
        "pages/comparisons/frameworks.md",
        "pages/contributing/runtime.md"
      ],
      groups_for_extras: [
        "Getting started": ~r"/getting-started/",
        "Understand Popcorn": ~r"/concepts/",
        Guides: ~r"/guides/",
        Reference: ~r"/reference/",
        "Migration and comparisons": ~r"/(?:migration|comparisons)/",
        Contributing: ~r"/contributing/"
      ],
      formatters: ["html"],
      source_url_pattern: "#{@github}/blob/v#{@version}/popcorn/elixir/%{path}#L%{line}",
      source_ref: "v#{@version}"
    ]
    |> ExDocJs.configure(
      entry_points: ["../js/src/index.ts"],
      tsconfig: "../js/tsconfig.json",
      root_module: "JS"
    )
  end

  defp deps do
    [
      {:req, ">= 0.5.0", optional: true},
      {:ex_doc, github: "software-mansion-labs/ex_doc", only: [:dev, :test], runtime: false},
      {:ex_doc_js, github: "software-mansion-labs/ex_doc_js", only: [:dev, :test], runtime: false}
    ]
  end
end
