import Config

config :fission_lib,
  out_path: "static/iex_wasm.avm",
  start_module: IexWasm,
  add_tracing: true
