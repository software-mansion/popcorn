import Config

config :popcorn,
  start_module: HelloPopcorn,
  runtime_source: {:path, "../../../AtomVM"},
  out_dir: "static/wasm"
