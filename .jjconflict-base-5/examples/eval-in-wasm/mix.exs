defmodule EvalInWasm.MixProject do
  use Mix.Project

  def project do
    [
      app: :eval_in_wasm,
      version: "0.1.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: [
        build: ["deps.get", "compile", &pnpm_install/1, &build_js/1],
        dev: ["build", &serve/1]
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
      {:playwright, "~> 1.49.1-alpha.2", runtime: false, only: :test},
      # playwright pins cowlib ~> 2.7.0 which fails to compile on OTP 28
      {:cowlib, "~> 2.13", override: true, runtime: false, only: :test}
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

  defp serve(_) do
    task = Path.expand("../../popcorn/utils/popcorn_server.ex", __DIR__)
    Code.require_file(task)
    Mix.Tasks.Popcorn.Server.run(["--port", "5173", "--dir", "dist"])
  end
end
