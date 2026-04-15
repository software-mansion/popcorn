import Config

config :immortal_grid, ImmortalGridWeb.Endpoint,
  http: [ip: {0, 0, 0, 0}, port: String.to_integer(System.get_env("PORT") || "4000")],
  check_origin: false,
  code_reloader: true,
  debug_errors: true,
  secret_key_base: "immortalgriddevkeybase1234567890immortalgriddevkeybase1234567890xxx",
  watchers: [
    node: [
      "build.mjs",
      "--watch",
      cd: Path.expand("../assets", __DIR__),
      env: %{"MIX_BUILD_PATH" => Mix.Project.build_path()}
    ],
    tailwind: {Tailwind, :install_and_run, [:immortal_grid, ~w(--watch)]}
  ]

config :immortal_grid, ImmortalGridWeb.Endpoint,
  live_reload: [
    web_console_logger: true,
    patterns: [
      ~r"priv/static/(?!uploads/).*(js|css|png|jpeg|jpg|gif|svg)$",
      ~r"priv/gettext/.*(po)$",
      ~r"lib/immortal_grid_web/(?:controllers|live|components|router)/?.*\.(ex|heex)$"
    ]
  ]

config :immortal_grid, dev_routes: true

config :logger, :default_formatter, format: "[$level] $message\n"

config :phoenix, :stacktrace_depth, 20
config :phoenix, :plug_init_mode, :runtime

config :phoenix_live_view,
  debug_heex_annotations: true,
  debug_attributes: true,
  enable_expensive_runtime_checks: true

config :swoosh, :api_client, false
