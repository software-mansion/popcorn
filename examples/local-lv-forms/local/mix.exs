defmodule Local.MixProject do
  use Mix.Project

  def project do
    [
      app: :local,
      version: "0.1.0",
      elixir: "~> 1.19",
      start_permanent: Mix.env() == :prod,
      deps_path: "../deps",
      lockfile: "../mix.lock",
      deps: deps(),
      compilers: Mix.compilers()
    ]
  end

  def cli do
    [default_target: :wasm]
  end

  # The local views use Ecto changesets for their forms, which need no
  # processes, so Ecto and Phoenix.Ecto are included applications: loaded
  # (and packaged by Popcorn), but not started.
  def application do
    [extra_applications: [:logger], included_applications: [:ecto, :phoenix_ecto]] ++
      app_mod(Mix.target())
  end

  defp app_mod(:host), do: []
  defp app_mod(_), do: [mod: {Local.Application, []}]

  defp deps do
    [
      {:gettext, "~> 1.0"},
      {:ecto, "~> 3.12", runtime: false},
      {:phoenix_ecto, "~> 4.6", runtime: false},
      {:local_live_view, path: "../../../local-live-view"}
    ]
  end
end
