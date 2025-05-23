defmodule Popcorn.MixProject do
  use Mix.Project

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
      version: "0.1.0",
      elixir: "1.17.3",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      aliases: [
        compile: ["compile", &download_artifacts/1, &patch/1],
        lint: [
          "format --check-formatted",
          "deps.unlock --check-unused",
          "credo",
          "deps.compile",
          "compile --force --warnings-as-errors",
          "docs --warnings-as-errors",
          "dialyzer"
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
      files: ["lib", "priv", "patches", "mix.exs", "README*", "LICENSE*", "src"],
      links: %{
        "GitHub" => "https://github.com/swmansion/popcorn",
        "Popcorn website" => "https://popcorn.swmansion.com"
      }
    ]
  end

  defp docs do
    [
      main: "readme",
      extras: ["README.md", "LICENSE"],
      formatters: ["html"]
    ]
  end

  defp download_artifacts(_args) do
    alias Popcorn.Utils.Download

    {:url, url} =
      Application.get_env(
        :popcorn,
        :runtime,
        {:url, "https://popcorn.swmansion.com/simple_repl/wasm/"}
      )

    dir = Path.join(Mix.Project.app_path(), "atomvm_artifacts/wasm")
    File.mkdir_p!(dir)
    artifacts = ["AtomVM.wasm", "AtomVM.mjs"]
    paths = Enum.map(artifacts, &Path.join(dir, &1))

    unless Enum.all?(paths, &File.exists?/1) do
      try do
        Download.start_inets_profile()
        Enum.each(artifacts, fn name -> download_artifact(url, dir, name) end)
      after
        Download.stop_inets_profile()
      end
    end
  end

  defp download_artifact(url, dir, name) do
    alias Popcorn.Utils.Download
    path = Path.join(dir, name)

    with {:ok, _stream} <-
           Download.download("#{url}/#{name}", File.stream!(path <> ".download")) do
      File.rename!(path <> ".download", path)
    else
      {:error, reason} ->
        IO.warn("""
        Couldn't download #{name}, reason: #{reason}, please use mix popcorn.build_runtime to build from source
        """)
    end
  end

  defp patch(_args) do
    Popcorn.Build.build()
  end

  defp deps do
    [
      # {:atomvm_packbeam, github: "atomvm/atomvm_packbeam"},
      {:jason, "~> 1.4"},
      {:ex_doc, "~> 0.34", only: :dev, runtime: false, warn_if_outdated: true},
      {:dialyxir, ">= 0.0.0", only: [:dev, :test], runtime: false},
      {:credo, ">= 0.0.0", only: [:dev, :test], runtime: false}
    ]
  end
end
