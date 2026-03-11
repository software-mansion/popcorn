defmodule EvalInWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :eval_in_wasm,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build: ["deps.get", &pnpm_install/1, "popcorn.cook", &build_js/1],
        dev: ["build", "popcorn.server"]
      ]
    ]
  end

  def application do
    [
      extra_applications: [],
      mod: {EvalInWasm.Application, []}
    ]
  end

  defp deps do
    [
      {:popcorn, path: "../../popcorn/elixir"},
      {:playwright,
       github: "membraneframework-labs/playwright-elixir", runtime: false, only: :test}
    ]
  end

  defp pnpm_install(_) do
    {_, 0} =
      System.cmd("pnpm", ["install"],
        cd: File.cwd!(),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
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
