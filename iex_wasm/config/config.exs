import Config

config :fission_lib,
  out_path: "static/elixir_modules.avm",
  start_module: ElixirModules,
  add_tracing: true,
  avm_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}
