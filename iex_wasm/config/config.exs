import Config

string_to_bool = fn
  "true" -> true
  "1" -> true
  "0" -> false
  "false" -> false
end

include_tracing =
  case System.get_env("EX_TRACING") do
    nil -> config_env() != :prod
    option -> string_to_bool.(option)
  end

config :fission_lib,
  out_path: "static/app.avm",
  start_module: App,
  add_tracing: include_tracing,
  avm_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}

build_test_modules = "EX_MODULES" |> System.get_env("false") |> string_to_bool.()

if build_test_modules do
  config :fission_lib,
    out_path: "static/elixir_modules.avm",
    start_module: ElixirModules
end
