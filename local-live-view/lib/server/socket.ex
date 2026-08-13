defmodule LocalLiveView.Socket do
  @moduledoc false

  # The `Phoenix.Socket` local live views connect to.

  # It carries `LocalLiveView.Channel`, which backs
  # `LocalLiveView.mirror_sync/2`. Mount it in your endpoint:

  # ```
  # socket "/llv_socket", LocalLiveView.Socket,
  #   websocket: [connect_info: [session: @session_options]]
  # ```

  # The socket is only needed by views that sync assigns to a mirror; a purely
  # local view never connects. `mix llv.install` adds the entry above
  # automatically.

  # Connections are rejected unless the client sends a CSRF token matching the
  # one in the session, so `connect_info` must carry the session.

  use Phoenix.Socket

  channel("llv:*", LocalLiveView.Channel)

  @impl true
  def connect(%{"_csrf_token" => client_csrf_token}, socket, %{session: session}) do
    session_csrf_state = Plug.CSRFProtection.dump_state_from_session(session["_csrf_token"])

    if Plug.CSRFProtection.valid_state_and_csrf_token?(session_csrf_state, client_csrf_token) do
      {:ok, socket}
    else
      :error
    end
  end

  def connect(_params, _socket, _connect_info) do
    :error
  end

  @impl true
  def id(_socket), do: nil
end
