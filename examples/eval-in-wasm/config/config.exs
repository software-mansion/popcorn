import Config

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end

if config_env() == :test do
  config :playwright, LaunchOptions, devtools: System.get_env("PLAYWRIGHT_DEBUG") == "true"
end
