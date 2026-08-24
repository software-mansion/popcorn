defmodule LocalLiveView.Endpoint do
  @moduledoc false
  # A real Phoenix endpoint started with server: false inside the WASM
  # runtime. Its supervision tree runs only Phoenix.Config (the ETS table
  # behind config/1), the url-cache warmup and the channel PoolSupervisor of
  # the socket below — no HTTP server, no pubsub, no static machinery.
  # Two AtomVM gaps are papered over in lib/stubs: Phoenix.Endpoint.Supervisor
  # (no :re engine there) and Phoenix.Config (no ets enumeration).
  #
  # Runtime config (secret_key_base, live_view signing) is passed by
  # LocalLiveView.Application at start; see endpoint_config/0 there.
  use Phoenix.Endpoint, otp_app: :local_live_view

  # The path is a dead label — nothing routes HTTP here. partitions: 1
  # because the single-scheduler runtime has no contention to spread across
  # supervisors; drainer: false skips the shutdown drainer (no server, no
  # draining).
  socket("/llv-popcorn", Phoenix.LiveView.Socket, partitions: 1, drainer: false)
end
