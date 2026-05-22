import Config

config :burrito, BurritoWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: String.to_integer(System.get_env("PORT") || "4000")],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "bUrR1t0S3cr3tK3yB4s3F0rD3vOnly!!PleaseChangeInProd1234567890abcd",
  watchers: [
    node: [
      "build.mjs",
      "--watch",
      cd: Path.expand("../assets", __DIR__),
      env: %{"MIX_BUILD_PATH" => Mix.Project.build_path()}
    ],
    tailwind: {Tailwind, :install_and_run, [:burrito, ~w(--watch)]}
  ]

config :burrito, dev_routes: true

config :logger, :default_formatter, format: "[$level] $message\n"

config :phoenix, :stacktrace_depth, 20

config :phoenix, :plug_init_mode, :runtime

config :phoenix_live_view,
  debug_heex_annotations: true,
  debug_attributes: true,
  enable_expensive_runtime_checks: true

config :swoosh, :api_client, false
