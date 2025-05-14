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
      elixirc_options: [no_warn_undefined: [:emscripten]],
      aliases: [compile: ["compile", &patch/1]],
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_env), do: ["lib"]

  defp patch(_args) do
    Popcorn.Build.build()
  end

  defp deps do
    [
      {:atomvm_packbeam, github: "atomvm/atomvm_packbeam"},
      {:jason, "~> 1.4"},
      {:ex_doc, "~> 0.34", runtime: false, warn_if_outdated: true},
    ]
  end
end
