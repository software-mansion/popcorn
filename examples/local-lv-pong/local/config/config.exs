import Config

# How `mix llv.build` packages this project, see `mix help popcorn.cook`
config :popcorn,
  # Web assets, which the host app serves, and generators' templates
  exclude_priv: ["static", phoenix: "templates", local_live_view: "*"],
  # Removes the code the local views don't use
  treeshake: []

if config_env() == :prod do
  # Smaller, but too slow for the watcher to rebuild with on every change, and
  # stack traces show no line numbers
  config :popcorn, brotli_effort: :max, treeshake: [line_info: false]
end
