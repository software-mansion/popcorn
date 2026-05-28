import Config

config :burrito, BurritoWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 4002],
  secret_key_base: "bUrR1t0S3cr3tK3yB4s3F0rT3stOnly!!PleaseChangeInProd1234567890xyz",
  server: false

config :logger, level: :warning

config :phoenix, :plug_init_mode, :runtime
