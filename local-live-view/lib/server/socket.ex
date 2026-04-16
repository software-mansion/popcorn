defmodule LocalLiveView.Socket do
  use Phoenix.Socket

  channel("llv:*", LocalLiveView.Channel)

  def connect(_params, socket, _connect_info) do
    {:ok, socket}
  end

  def id(_socket), do: nil
end
