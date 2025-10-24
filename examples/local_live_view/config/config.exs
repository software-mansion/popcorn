import Config

config :popcorn,
  out_dir: "static/local_live_view/wasm",
  add_tracing: false,
  extra_apps: [:crypto, :eex]
