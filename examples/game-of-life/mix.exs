defmodule GameOfLife.MixProject do
  use Mix.Project

  def project do
    [
      app: :game_of_life,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      extra_applications: [:logger],
      mod: {GameOfLife.Application, []}
    ]
  end

  defp deps do
    [{:popcorn, path: "../../popcorn/elixir"}]
  end

  defp aliases do
    [
      dev: ["compile", &build_assets/1, &serve/1]
    ]
  end

  defp build_assets(_) do
    {_, 0} =
      System.cmd("pnpm", ["run", "build"],
        cd: Path.join(File.cwd!(), "assets"),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
  end

  defp serve(_) do
    task = Path.expand("../../popcorn/utils/popcorn_server.ex", __DIR__)
    Code.require_file(task)
    Mix.Tasks.Popcorn.Server.run(["--port", "4000", "--dir", "dist"])
  end
end
