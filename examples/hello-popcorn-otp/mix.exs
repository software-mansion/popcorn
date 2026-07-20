defmodule HelloPopcornOtp.MixProject do
  use Mix.Project

  def project do
    [
      app: :hello_popcorn_otp,
      version: "0.1.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      aliases: aliases()
    ]
  end

  def application do
    [
      # Only apps present in the OTP runtime manifest (kernel, stdlib,
      # compiler, elixir, logger) may be listed — they ship as tarballs baked
      # into the OTP/WASM build. Other apps make the asset packer fail with a
      # missing dependency.
      extra_applications: [:logger],
      mod: {HelloPopcornOtp.Application, []}
    ]
  end

  defp deps do
    [{:popcorn_otp, path: "../../otp/elixir"}]
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
    task = Path.expand("../../otp/utils/popcorn_server.ex", __DIR__)
    Code.require_file(task)
    Mix.Tasks.Popcorn.Server.run(["--port", "5173", "--dir", "dist"])
  end
end
