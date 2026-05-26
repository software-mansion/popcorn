import Config

config :popcorn,
  out_dir: "dist/wasm",
  treeshake: System.get_env("POPCORN_TREESHAKE") != nil

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end
