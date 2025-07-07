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

config :popcorn,
  out_dir: "static/wasm",
  add_tracing: include_tracing

if path = System.get_env("ATOMVM_SOURCE_PATH") do
  config :popcorn, runtime_source: {:path, path}
end

if File.exists?("#{__DIR__}/config.secret.exs") do
  import_config "config.secret.exs"
end

config :playwright, LaunchOptions, devtools: System.get_env("PLAYWRIGHT_DEBUG") == "true"
