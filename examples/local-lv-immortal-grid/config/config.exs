import Config

config :immortal_grid,
  generators: [timestamp_type: :utc_datetime]

config :immortal_grid, ImmortalGridWeb.Endpoint,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: ImmortalGridWeb.ErrorHTML, json: ImmortalGridWeb.ErrorJSON],
    layout: false
  ],
  pubsub_server: ImmortalGrid.PubSub,
  live_view: [signing_salt: "iGr1dSalt"]

config :immortal_grid, ImmortalGrid.Mailer, adapter: Swoosh.Adapters.Local

config :tailwind,
  version: "4.1.7",
  immortal_grid: [
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
