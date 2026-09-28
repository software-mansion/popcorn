import Config

config :llv_integration, LlvIntegrationWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [formats: [html: LlvIntegrationWeb.ErrorHTML], layout: false],
  pubsub_server: LlvIntegration.PubSub,
  live_view: [signing_salt: "llv-integration"],
  secret_key_base: String.duplicate("llv-integration-secret-key-base", 3)

config :esbuild,
  version: "0.25.4",
  llv_integration: [
    args:
      ~w(js/app.js --bundle --format=esm --target=es2022 --outdir=../priv/static/assets/js --alias:local_live_view=#{Path.expand("../../priv/static/local_live_view.js", __DIR__)}),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]

config :logger, :default_formatter, format: "$time [$level] $message\n"

config :phoenix, :json_library, Jason

import_config "#{config_env()}.exs"
