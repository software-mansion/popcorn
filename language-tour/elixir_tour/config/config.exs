import Config

config :popcorn,
  start_module: ElixirTour,
  out_dir: "../public/wasm"

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end
