import Config

config :popcorn,
  out_dir: "static/wasm"

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end
