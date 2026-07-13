defmodule TestEntrypoint.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_entrypoint,
      version: "0.1.0",
      deps_path: "../../../../elixir/deps",
      lockfile: "../../../../elixir/mix.lock",
      deps: deps()
    ]
  end

  def application do
    [
      mod: {:test_entrypoint_app, []},
      extra_applications: [:elixir]
    ]
  end

  defp deps do
    deps = [{:popcorn, path: "../../../../elixir"}]

    if System.get_env("POPCORN_E2E_REQ") == "1" do
      [{:req, ">= 0.5.0"} | deps]
    else
      deps
    end
  end
end
