defmodule LocalLiveView.Endpoint do
  @moduledoc false
  # A fake endpoint for LiveView to be happy

  use Phoenix.Endpoint, otp_app: :local_live_view

  socket("/llv-popcorn", Phoenix.LiveView.Socket, partitions: 1, drainer: false)
end
