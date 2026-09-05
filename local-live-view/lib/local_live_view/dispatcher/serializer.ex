defmodule LocalLiveView.Dispatcher.Serializer do
  @moduledoc false
  # The dispatcher and the LiveView channel run in the same runtime: channel
  # traffic stays plain Elixir terms. "Encoding" only wraps the term in the
  # {:socket_push, opcode, message} envelope Phoenix.Socket's internals
  # destructure; the message itself crosses untouched, and "decoding" is the
  # identity.
  #
  # The behaviour declaration matters beyond documentation: the channel calls
  # `serializer.encode!(...)` on a variable module, which treeshaking cannot
  # see — but it keeps behaviour callbacks of literally-referenced modules
  # (the dispatcher names this module in the socket connect options).
  @behaviour Phoenix.Socket.Serializer

  @impl true
  def encode!(message), do: {:socket_push, :binary, message}

  @impl true
  def fastlane!(message), do: {:socket_push, :binary, message}

  @impl true
  def decode!(message, _opts), do: message
end
