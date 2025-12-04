defmodule FormDemoWeb.LocalChannel do
  use Phoenix.Channel

  @impl true
  def join("llv_topic", _params, socket) do
    {:ok, socket}
  end

  @impl true
  def handle_in("llv_message", %{"view" => view, "payload" => payload}, socket) do
#    YourAppWeb.Endpoint.broadcast!(
#      "room:lobby", "message_received", %{sender: "Server", text: body}
#    )
    IO.inspect("LLV MESSAGE RECEIVED\nVIEW: #{inspect(view)} PAYLOAD: #{inspect(payload)}")
    {:noreply, socket}
  end
end
