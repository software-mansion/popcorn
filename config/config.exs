import Config

config :popcorn,
  add_tracing: false,
  runtime_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}

#  ref: "ba6bd984ca3d8b2cb00f222625c8c10f2b873d80"}

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :popcorn, runtime_path: ...
  import_config "config.secret.exs"
end
