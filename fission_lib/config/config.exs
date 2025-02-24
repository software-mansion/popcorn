import Config

config :fission_lib,
  add_tracing: false,
  avm_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git", ref: "asan-opt"}

if File.exists?("#{__DIR__}/config.secret.exs") do
  # Put the following configuration in config.secret.exs:
  # config :fission_lib, atomvm_path: ...
  import_config "config.secret.exs"
end
