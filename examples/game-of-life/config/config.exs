import Config

config :popcorn,
  add_tracing: false,
  extra_apps: [:eex],
  out_dir: "dist/wasm"

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :popcorn, runtime_path: ...
  import_config "config.secret.exs"
end
