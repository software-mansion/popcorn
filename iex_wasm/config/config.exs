import Config

string_to_bool = fn
  "true" -> true
  "1" -> true
  "0" -> false
  "false" -> false
end

include_tracing =
  case System.get_env("EX_TRACING") do
    nil -> config_env() not in [:prod, :test]
    option -> string_to_bool.(option)
  end

config :fission_lib,
  out_path: "static/app.avm",
  start_module: App.Application,
  add_tracing: include_tracing,
  avm_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}

if config_env() == :test do
  config :playwright, LaunchOptions, %{headless: true}
end
