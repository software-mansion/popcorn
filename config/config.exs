import Config

config :popcorn, extra_apps: [:asn1, :ssl, :crypto, :public_key]

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :popcorn, runtime_path: ...
  import_config "config.secret.exs"
end
