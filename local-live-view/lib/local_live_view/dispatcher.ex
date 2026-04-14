defmodule LocalLiveView.Dispatcher do
  @moduledoc false

  #  This process dispatches events to different LocalLiveViews present on the page.
  #
  #  It uses Popcorn API and registers as a main process to handle wasm messages inside Popcorn runtime.
  #
  #  Uses GenServer.

  use GenServer
  alias Phoenix.LiveView.Session
  import Popcorn.Wasm
  alias Popcorn.Wasm
  alias LocalLiveView.Message
  @process_name :main

  @doc false
  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.ready(@process_name)
    {:ok, %{views: []}}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"id" => id, "event" => "llv_attrs_update", "payload" => attrs}},
         state
       ) do
    Map.get(state.views, id)
    |> send(%Message{payload: attrs, event: "attrs_update"})

    {:resolve, :ok, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"id" => id, "event" => "llv_reconnected"}},
         state
       ) do
    Map.get(state.views, id)
    |> send(%Message{event: "llv_reconnected", payload: %{}})

    {:resolve, :ok, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"id" => id, "event" => _type, "payload" => payload}},
         state
       ) do
    Map.get(state.views, id)
    |> send(%Message{payload: payload, event: "event"})

    {:resolve, :ok, state}
  end

  defp handle_wasm({:wasm_call, %{"views" => views}}, state) do
    started =
      views
      |> Enum.map(fn %{"view" => view_string, "id" => id} ->
        view = ("Elixir." <> view_string) |> String.to_existing_atom()
        start_local_live_view(view, id)
      end)
      |> Enum.filter(fn result -> result != nil end)

    views_map = Map.new(started, fn {id, pid, _rendered} -> {id, pid} end)

    initial_rendered = Map.new(started, fn {id, _pid, rendered} -> {id, rendered} end)

    {:resolve, initial_rendered, %{state | views: views_map}}
  end

  defp start_local_live_view(view, id) do
    params = %{
      "session" => %Session{view: view},
      "llv_id" => id
    }

    ref = make_ref()

    with {:ok, pid} <- LocalLiveView.Server.start_llv_process() do
      send(pid, {LocalLiveView.Server, params, {self(), ref}, %Phoenix.Socket{}})

      receive do
        {^ref, {:ok, rendered}} ->
          {id, pid, rendered}

        {^ref, {:error, _reply}} ->
          nil
      end
    end
  end
end
