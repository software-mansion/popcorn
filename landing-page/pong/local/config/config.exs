import Config

# How `mix llv.build` packages this project, see `mix help popcorn.cook`
config :popcorn,
  # Web assets, which the host app serves, and generators' templates
  exclude_priv: ["static", phoenix: "templates", local_live_view: "*"]
