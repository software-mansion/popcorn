defmodule GameOfLife.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_of_life,
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
      extra_applications: [:eex],
      mod: {GameOfLife.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"}
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
