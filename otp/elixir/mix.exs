defmodule PopcornOtp.MixProject do
  use Mix.Project

  @version "0.1.0"
  @github "https://github.com/software-mansion/popcorn"

  def project do
    [
      app: :popcorn_otp,
      version: @version,
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      elixirc_paths: elixirc_paths(Mix.env()),
      deps: deps(),
      description: "Elixir API for Popcorn's OTP/BEAM WebAssembly runtime",
      package: package(),
      name: "Popcorn OTP",
      docs: docs(),
      source_url: @github,
      homepage_url: "https://popcorn.swmansion.com"
    ]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp package do
    [
      maintainers: ["Software Mansion"],
      licenses: ["Apache-2.0"],
      files: ["lib", "mix.exs", "README.md"],
      links: %{
        "GitHub" => @github,
        "Popcorn website" => "https://popcorn.swmansion.com"
      }
    ]
  end

  defp docs do
    [
      main: "Popcorn.Wasm",
      formatters: ["html"],
      source_ref: "v#{@version}"
    ]
  end

  defp deps do
    [
      {:ex_doc, "~> 0.34", only: [:dev, :test], runtime: false}
    ]
  end
end
