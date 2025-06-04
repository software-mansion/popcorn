import Config

root = "#{__DIR__}/../"

config :popcorn,
  add_tracing: false,
  out_dir: "#{root}/static/wasm"

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :popcorn, runtime_path: ...
  import_config "config.secret.exs"
end
