import Config

root = "#{__DIR__}/../"

config :fission_lib,
  add_tracing: false,
  out_path: "#{root}/static/wasm/bundle.avm",
  start_module: GameOfLife

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :fission_lib, atomvm_path: ...
  import_config "config.secret.exs"
end
