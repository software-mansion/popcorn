import Config

config :popcorn,
  runtime: [{:path, Path.join(__DIR__, "../popcorn_runtime_source/artifacts/$target")}]

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :popcorn, runtime_path: ...
  import_config "config.secret.exs"
end
