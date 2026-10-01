import Config

# The endpoint only serves while test/e2e_test.exs runs, see there.
config :llv_integration, LlvIntegrationWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4904],
  server: false

config :logger, level: :warning

config :phoenix, :plug_init_mode, :runtime
