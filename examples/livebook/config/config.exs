import Config

# Popcorn evaluates only this file, and the browser can read all of it. These are
# the settings Livebook's own config and `Livebook.config_runtime/0` would make.

config :popcorn,
  distribution: true,
  exclude_priv: [{:livebook, "static"}]

embedded = %{__struct__: Livebook.Runtime.Embedded, server_pid: nil}

config :livebook, LivebookWeb.Endpoint,
  url: [host: "localhost", path: "/"],
  pubsub_server: Livebook.PubSub,
  live_view: [signing_salt: "livebook"],
  render_errors: [formats: [html: LivebookWeb.ErrorHTML], layout: false],
  secret_key_base: String.duplicate("popcorn-livebook-not-secret-", 3),
  server: false

config :livebook,
  agent_name: "default",
  allowed_uri_schemes: [],
  app_service_name: nil,
  app_service_url: nil,
  apps_banner: nil,
  authentication: :disabled,
  aws_credentials: false,
  feature_flags: [],
  force_ssl_host: nil,
  learn_notebooks: [],
  plugs: [],
  rewrite_on: [],
  shutdown_callback: nil,
  teams_auth: nil,
  teams_url: "https://teams.livebook.dev",
  github_release_info: %{repo: "livebook-dev/livebook", version: "0.19.10"},
  update_instructions_url: nil,
  within_iframe: false,
  k8s_kubeconfig_pipeline: Kubereq.Kubeconfig.Default,
  random_boot_id: <<0, 0, 1>>,
  log_format: :text,
  cookie: :popcorn_livebook,
  iframe_port: 8081,
  # Outputs' iframes load from Livebook's origin, where the Service Worker sends
  # their requests to the VM. Livebook's usual separate origin isolates them.
  iframe_url: "/iframe/v6.html",
  default_runtime: embedded,
  default_app_runtime: embedded,
  runtime_modules: [Livebook.Runtime.Embedded, Livebook.Runtime.Attached]

config :livebook, Livebook.Apps.Manager, retry_backoff_base_ms: 5_000

# memsup and the other monitors run OS programs; LivebookWasm.Memsup stands in
config :os_mon,
  start_cpu_sup: false,
  start_disksup: false,
  start_memsup: false,
  start_os_sup: false

config :logger, level: :info
config :phoenix, :json_library, JSON
config :mime, :types, %{"audio/m4a" => ["m4a"], "text/plain" => ["livemd"]}
