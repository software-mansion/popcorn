import Config

string_to_bool = fn
  "true" -> true
  "1" -> true
  "0" -> false
  "false" -> false
end

include_tracing =
  case System.get_env("EX_TRACING") do
    nil -> false
    option -> string_to_bool.(option)
  end

config :fission_lib,
  out_path: "static/wasm/app.avm",
  start_module: App.Application,
  add_tracing: include_tracing,
  avm_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end
