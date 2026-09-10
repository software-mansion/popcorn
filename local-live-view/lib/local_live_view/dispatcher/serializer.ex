defmodule LocalLiveView.Dispatcher.Serializer do
  @moduledoc false
  # Serializer for transport frames sent between JS and LV channels
  # through the dispatcher.
  # There's no real serialization here, only reshaping the transport
  # frames as Phoenix requires.

  @behaviour Phoenix.Socket.Serializer

  alias Phoenix.Socket.{Message, Reply}

  @impl true
  def encode!(%Reply{} = reply) do
    {:socket_push, :binary,
     %{
       topic: reply.topic,
       event: "phx_reply",
       payload: %{status: reply.status, response: reply.payload},
       ref: reply.ref,
       join_ref: reply.join_ref
     }}
  end

  def encode!(%Message{} = message) do
    {:socket_push, :binary, Map.from_struct(message)}
  end

  def encode!(message), do: {:socket_push, :binary, message}

  @impl true
  def fastlane!(message), do: {:socket_push, :binary, message}

  @impl true
  def decode!(frame, _opts) do
    %Message{
      topic: frame["topic"],
      event: frame["event"],
      payload: frame["payload"],
      ref: frame["ref"],
      join_ref: frame["join_ref"]
    }
  end
end
