defmodule TestServer.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_server,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [],
      mod: {TestServer.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../../../"}
    ]
  end
end
