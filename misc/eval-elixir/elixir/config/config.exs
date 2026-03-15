import Config

config :popcorn, out_dir: "out"

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end
