import Config

config :popcorn,
  start_module: LocalLiveView,
  out_dir: "static/wasm",
  add_tracing: false
