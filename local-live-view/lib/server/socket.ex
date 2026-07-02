defmodule LocalLiveView.Socket do
  use Phoenix.Socket

  channel("llv:*", LocalLiveView.Channel)

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

  def id(_socket), do: nil
end
