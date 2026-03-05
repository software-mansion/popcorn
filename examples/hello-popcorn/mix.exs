defmodule HelloPopcorn.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello_popcorn,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build_assets: ["popcorn.cook", &build_js/1],
        dev: ["build_assets", "popcorn.server"]
      ]
    ]
  end

  def application do
    [
      extra_applications: [],
      mod: {HelloPopcorn.Application, []}
    ]
  end

  defp deps do
    [
      # {:popcorn, github: "software-mansion/popcorn"}
      {:popcorn, path: "../../popcorn/elixir"},
      # playwright will be started manually
      {:playwright,
       github: "membraneframework-labs/playwright-elixir", runtime: false, only: :test}
    ]
  end

  defp build_js(_) do
    {_, 0} =
      System.cmd("pnpm", ["run", "build"],
        cd: Path.join(File.cwd!(), "assets"),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
  end
end
