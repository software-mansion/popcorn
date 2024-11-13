import Config

config :fission_lib,
  out_path: "static/iex_wasm.avm",
  start_module: IExWASM,
  add_tracing: true
