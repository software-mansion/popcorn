import Config

config :immortal_grid, ImmortalGridWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "immortalgridtestkeybase1234567890immortalgridtestkeybase1234567890xxx",
  server: false

config :immortal_grid, ImmortalGrid.Mailer, adapter: Swoosh.Adapters.Test

config :swoosh, :api_client, false

config :logger, level: :warning

config :phoenix, :plug_init_mode, :runtime

config :phoenix_live_view,
  enable_expensive_runtime_checks: true
