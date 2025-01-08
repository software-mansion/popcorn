import Config

config :fission_lib,
  out_path: "static/iex_wasm.avm",
  start_module: IExWASM,
  add_tracing: false,
  avm_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}
