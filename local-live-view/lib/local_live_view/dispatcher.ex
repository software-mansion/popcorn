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
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"action" => "reconnected", "id" => id}},
         state
       ) do
    send_to_view(state, id, %Message{event: "llv_reconnected", payload: %{}})
    {:resolve, :ok, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"action" => "push", "id" => id, "payload" => payload}},
         state
       ) do
    send_to_view(state, id, %Message{event: "js_push", payload: payload})
    {:resolve, :ok, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"action" => "update_assigns", "id" => id, "assigns" => assigns}},
         state
       ) do
    send_to_view(state, id, %Message{event: "update_assigns", payload: assigns})
    {:resolve, :ok, state}
  end

  defp handle_wasm({:wasm_call, %{"action" => "destroy", "id" => id}}, state) do
    # The host LiveView removed a mount point. Stop its process and forget it.
    case Map.get(state.views, id) do
      nil -> :ok
      pid -> DynamicSupervisor.terminate_child(LocalLiveView.Server.Supervisor, pid)
    end

    {:resolve, :ok, %{state | views: Map.delete(state.views, id)}}
  end

  # This event may be fired multiple times for the same view from JS,
  # in such case we only handle the first event.
  defp handle_wasm({:wasm_call, %{"action" => "create", "id" => id} = msg}, state)
       when not is_map_key(state.views, id) do
    view = String.to_existing_atom("Elixir." <> Map.fetch!(msg, "view"))

    params =
      Map.take(msg, ~w"id assigns url url_params")
      |> Map.put("session", %Session{view: view})

    case start_local_live_view(params) do
      {:ok, pid, rendered} ->
        {:resolve, %{status: :ok, rendered: rendered}, put_in(state.views[id], pid)}

      :error ->
        {:resolve, %{status: :error}, state}
    end
  end

  defp handle_wasm({:wasm_call, %{"action" => "create"}}, state) do
    {:resolve, %{status: :error}, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"action" => "handle_params", "id" => id, "payload" => payload}},
         state
       ) do
    send_to_view(state, id, %Message{event: "handle_params", payload: payload})
    {:resolve, :ok, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"action" => "event", "id" => id, "payload" => payload}},
         state
       ) do
    send_to_view(state, id, %Message{payload: payload, event: "event"})
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
    send(pid, {LocalLiveView.Server, params, {self(), ref}, %Phoenix.Socket{}})

    receive do
      {^ref, {:ok, rendered}} -> {:ok, pid, rendered}
      {^ref, {:error, _reply}} -> :error
    end
  end
end
