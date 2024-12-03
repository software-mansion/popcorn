import Config

config :fission_lib, add_tracing: false

if Mix.env() == :test do
  # Put the following configuration in config.secret.exs:
  # config :fission_lib, atomvm_path: ...
  import_config "config.secret.exs"
end
