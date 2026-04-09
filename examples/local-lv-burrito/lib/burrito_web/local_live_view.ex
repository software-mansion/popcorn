defmodule BurritoWeb.LocalLiveView do
  @moduledoc false

  import Phoenix.LiveView, only: [push_event: 3]

  def push_to_local(socket, view, payload) do
    push_event(socket, "llv_server_message", %{"view" => view, "payload" => payload})
  end
end
