import Config

config :immortal_grid, ImmortalGridWeb.Endpoint,
  cache_static_manifest: "priv/static/cache_manifest.json"

config :swoosh, api_client: :none
config :swoosh, local: false

config :logger, level: :info
