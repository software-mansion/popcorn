defmodule PhxWasm.Endpoint do
  use Phoenix.Endpoint, otp_app: :phx_wasm

  @session_options [
    store: :cookie,
    key: "_phx_wasm_key",
    signing_salt: "spike-session",
    same_site: "Lax"
  ]

  socket "/live", Phoenix.LiveView.Socket,
    websocket: [connect_info: [session: @session_options]],
    longpoll: false

  plug Plug.RequestId

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.MethodOverride
  plug Plug.Head
  plug Plug.Session, @session_options
  plug PhxWasm.Router
end
