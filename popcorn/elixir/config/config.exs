import Config

config :logger, :default_formatter, metadata: [:app_name]

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end
