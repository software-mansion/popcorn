import Config

config :popcorn,
  add_tracing: false,
  runtime_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :popcorn, runtime_path: ...
  import_config "config.secret.exs"
end
