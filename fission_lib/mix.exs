defmodule FissionLib.MixProject do
  use Mix.Project

  def project do
    [
      app: :fission_lib,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:atomvm_packbeam, github: "atomvm/atomvm_packbeam"}
    ]
  end
end
