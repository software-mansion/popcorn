import Config

config :popcorn,
  start_module: HelloPopcorn,
  runtime_source: {:git, "https://github.com/software-mansion-labs/FissionVM.git", ref: "jgonet/js-api-batch"},
  out_dir: "static/wasm"
