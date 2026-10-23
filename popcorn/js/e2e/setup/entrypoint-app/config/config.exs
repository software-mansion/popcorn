import Config

config :test_entrypoint, answer: 42
config :logger, level: :warning
config :popcorn, strip: true

# The global setup also packages it without an entrypoint
if config_env() == :no_entrypoint do
  config :popcorn, app: nil
end
