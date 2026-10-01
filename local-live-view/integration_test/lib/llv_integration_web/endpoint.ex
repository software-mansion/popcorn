defmodule LlvIntegrationWeb.Endpoint do
  use Phoenix.Endpoint, otp_app: :llv_integration

  @session_options [
    store: :cookie,
    key: "_llv_integration_key",
    signing_salt: "llv-integration",
    same_site: "Lax"
  ]

  # The Wasm runtime needs a cross-origin isolated page.
  plug :put_wasm_security_headers

  socket "/live", Phoenix.LiveView.Socket, websocket: [connect_info: [session: @session_options]]

  socket "/llv_socket", LocalLiveView.Socket,
    websocket: [connect_info: [session: @session_options]]

  plug Plug.Static, at: "/", from: :llv_integration, only: ~w(assets)

  plug Plug.Parsers,
    parsers: [:urlencoded, :multipart, :json],
    pass: ["*/*"],
    json_decoder: Phoenix.json_library()

  plug Plug.Session, @session_options
  plug LlvIntegrationWeb.Router

  defp put_wasm_security_headers(conn, _opts) do
    conn
    |> put_resp_header("cross-origin-opener-policy", "same-origin")
    |> put_resp_header("cross-origin-embedder-policy", "require-corp")
  end
end
