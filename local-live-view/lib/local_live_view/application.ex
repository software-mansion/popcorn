defmodule LocalLiveView.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {LocalLiveView.Endpoint, endpoint_config()},
      LocalLiveView.Dispatcher
    ]

    opts = [strategy: :one_for_one, name: LocalLiveView.Supervisor]
    Supervisor.start_link(children, opts)
  end

  defp endpoint_config do
    [
      server: false,
      # Only used for signing some JS <-> Wasm messages. LiveView doesn't support
      # disabling the signing.
      secret_key_base: "local-live-view-browser-only-secret-key-base-000000000000000000",
      live_view: [signing_salt: "local-live-view", hibernate_after: :infinity]
    ]
  end
end
