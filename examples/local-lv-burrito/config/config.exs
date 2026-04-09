import Config

config :burrito,
  generators: [timestamp_type: :utc_datetime]

config :burrito, BurritoWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: BurritoWeb.ErrorHTML, json: BurritoWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: Burrito.PubSub,
  live_view: [signing_salt: "bUrR1t0Lv"]

config :burrito, Burrito.Mailer, adapter: Swoosh.Adapters.Local

config :tailwind,
  version: "4.1.7",
  burrito: [
    args: ~w(
      --input=assets/css/app.css
      --output=priv/static/assets/css/app.css
    ),
    cd: Path.expand("..", __DIR__)
  ]

config :logger, :default_formatter,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

config :phoenix, :json_library, Jason

import_config "#{config_env()}.exs"
