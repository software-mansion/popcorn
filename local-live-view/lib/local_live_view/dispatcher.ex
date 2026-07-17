defmodule LocalLiveView.Dispatcher do
  @moduledoc false

  #  This process dispatches events to different LocalLiveViews present on the page.
  #
  #  It uses Popcorn API and registers as a main process to handle wasm messages inside Popcorn runtime.
  #
  #  Uses GenServer.

  use GenServer
  import Popcorn.Wasm
  alias Popcorn.Wasm
  alias LocalLiveView.Message
  alias Phoenix.LiveView.Session
  @process_name :main

  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.ready(@process_name)
    {:ok, %{views: %{}}}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    {:wasm_call, msg, promise} = Wasm.parse_message!(raw_msg)

    case handle_wasm_call(msg, promise, state) do
      {:resolve, reply, state} ->
        Wasm.resolve(reply, promise)
        {:noreply, state}

      {:reject, reason, state} ->
        Wasm.reject(reason, promise)
        {:noreply, state}

      {:ignore, state} ->
        {:noreply, state}
    end
  end

  defp handle_wasm_call(%{"action" => "transport_frame", "id" => id} = msg, promise, state)
       when is_map_key(state.views, id) do
    send(state.views[id], %Message{event: msg["event"], payload: msg["payload"], promise: promise})

    {:ignore, state}
  end

  defp handle_wasm_call(%{"action" => "transport_frame"}, _promise, state) do
    {:reject, "view not mounted", state}
  end

  defp handle_wasm_call(%{"action" => "reconnected", "id" => id}, _promise, state) do
    send_to_view(state, id, %Message{event: "llv_reconnected", payload: %{}})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(%{"action" => "push", "id" => id, "payload" => payload}, _promise, state) do
    send_to_view(state, id, %Message{event: "js_push", payload: payload})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(
         %{"action" => "push_error", "id" => id, "payload" => payload},
         _promise,
         state
       ) do
    send_to_view(state, id, %Message{event: "push_error", payload: payload})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(
         %{"action" => "update_assigns", "id" => id, "assigns" => assigns},
         _promise,
         state
       ) do
    send_to_view(state, id, %Message{event: "update_assigns", payload: assigns})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(%{"action" => "destroy", "id" => id}, _promise, state) do
    # The host LiveView removed a mount point. Stop its process and forget it.
    case Map.get(state.views, id) do
      nil -> :ok
      pid -> DynamicSupervisor.terminate_child(LocalLiveView.Server.Supervisor, pid)
    end

    {:resolve, :ok, %{state | views: Map.delete(state.views, id)}}
  end

  # This event may be fired multiple times for the same view from JS,
  # in such case we only handle the first event.
  defp handle_wasm_call(%{"action" => "create", "id" => id} = msg, _promise, state)
       when not is_map_key(state.views, id) do
    view = String.to_atom("Elixir." <> Map.fetch!(msg, "view"))

    params =
      Map.take(msg, ~w"id assigns url url_params")
      |> Map.put("session", %Session{view: view})

    case start_local_live_view(params) do
      {:ok, pid} ->
        {:resolve, :ok, put_in(state.views[id], pid)}

      :error ->
        {:reject, "error creating LLV", state}
    end
  end

  defp handle_wasm_call(%{"action" => "create"}, _promise, state) do
    {:reject, "error creating LLV", state}
  end

  defp handle_wasm_call(
         %{"action" => "handle_params", "id" => id, "payload" => payload},
         _promise,
         state
       ) do
    send_to_view(state, id, %Message{event: "handle_params", payload: payload})
    {:resolve, :ok, state}
  end

  defp handle_wasm_call(
         %{"action" => "server_message", "id" => id, "payload" => payload},
         _promise,
         state
       ) do
    # Host-pushed llv_server_message: not a channel push, so there is no ack
    # to carry the render — the view handles it like an event and pushes any
    # resulting diff out-of-band (nil promise).
    send_to_view(state, id, %Message{event: "event", payload: payload})
    {:resolve, :ok, state}
  end

  # Deliver to a view's process, ignoring events for an id that isn't mounted
  # (e.g. an event that arrives just after the view was torn down). Without this,
  # send(nil, msg) would crash the dispatcher.
  defp send_to_view(state, id, message) do
    case Map.get(state.views, id) do
      nil -> :ok
      pid -> send(pid, message)
    end
  end

  defp start_local_live_view(params) do
    ref = make_ref()

    {:ok, pid} = LocalLiveView.Server.start_llv_process()
    send(pid, {LocalLiveView.Server, params, {self(), ref}})

    receive do
      {^ref, :ok} -> {:ok, pid}
      {^ref, {:error, _reply}} -> :error
    end
  end
end
