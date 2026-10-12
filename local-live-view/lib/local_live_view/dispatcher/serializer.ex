defmodule LocalLiveView.Dispatcher.Serializer do
  @moduledoc false
  # Serializer for transport frames sent between JS and LV channels
  # through the dispatcher.
  # Frames are JSON-encoded the way they are over LiveView's websocket, with
  # Phoenix's JSON library, so that payloads look the same to the LiveView
  # client.

  @behaviour Phoenix.Socket.Serializer

  alias Phoenix.Socket.{Broadcast, Message, Reply}

  @impl true
  def encode!(%Reply{} = reply) do
    push(%{
      topic: reply.topic,
      event: "phx_reply",
      payload: %{status: reply.status, response: reply.payload},
      ref: reply.ref,
      join_ref: reply.join_ref
    })
  end

  def encode!(%Message{} = message) do
    push(Map.from_struct(message))
  end

  @impl true
  def fastlane!(%Broadcast{} = broadcast) do
    push(%{topic: broadcast.topic, event: broadcast.event, payload: broadcast.payload})
  end

  @impl true
  def decode!(raw_frame, _opts) do
    frame = Phoenix.json_library().decode!(raw_frame)

    %Message{
      topic: frame["topic"],
      event: frame["event"],
      payload: frame["payload"],
      ref: frame["ref"],
      join_ref: frame["join_ref"]
    }
  end

  # The topic stays outside of the JSON for routing, see the dispatcher.
  defp push(%{topic: topic} = message) do
    {:socket_push, :text, {topic, Phoenix.json_library().encode!(message)}}
  end
end
