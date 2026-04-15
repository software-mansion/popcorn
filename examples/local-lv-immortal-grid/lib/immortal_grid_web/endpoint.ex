defmodule ImmortalGridWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :immortal_grid

  @session_options [
    store: :cookie,
    key: "_immortal_grid_key",
    signing_salt: "iGr1dSign",
    same_site: "Lax"
  ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: [connect_info: [session: @session_options]]

  plug Plug.Static,
    at: "/",
    from: :immortal_grid,
    gzip: not code_reloading?,
    only: ImmortalGridWeb.static_paths(),
    headers: [
      {"Access-Control-Allow-Origin", "*"},
      {"Cross-Origin-Opener-Policy", "same-origin"},
      {"Cache-Control", "public no-cache"},
      {"Cross-Origin-Embedder-Policy", "require-corp"}
    ]

  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
    plug Phoenix.CodeReloader
  end

  plug Phoenix.LiveDashboard.RequestLogger,
    param_key: "request_logger",
    cookie_key: "request_logger"

  plug Plug.RequestId
  plug Plug.Telemetry, event_prefix: [:phoenix, :endpoint]

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  plug ImmortalGridWeb.Router
end
