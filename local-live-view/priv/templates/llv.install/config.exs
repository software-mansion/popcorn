import Config

# How `mix llv.build` packages this project, see `mix help popcorn.cook`
config :popcorn,
  # Web assets, which the host app serves, and generators' templates
  exclude_priv: ["static", phoenix: "templates", local_live_view: "*"]

if config_env() == :prod do
  # Smaller, but too slow for the watcher to rebuild with on every change
  config :popcorn, brotli_effort: :max
end

# Tree-shaking removes the code the local views don't use, which makes the
# package much smaller, but it's experimental:
#
#     config :popcorn, treeshake: true
