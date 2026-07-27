defmodule TestEntrypoint.MixProject do
  use Mix.Project

  def project do
    [
      app: :test_entrypoint,
      version: "0.1.0",
      deps: [{:popcorn_otp, path: "../../../../elixir"}]
    ]
  end

  def application do
    [
      mod: {:test_entrypoint_app, []},
      extra_applications: [:elixir]
    ]
  end
end
