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
    Popcorn.Wasm.register(@process_name)
    {:ok, %{views: []}}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"event" => _type, "view" => view_string, "payload" => payload}},
         state
       ) do
    view = Module.safe_concat([view_string])

    Map.get(state.views, view)
    |> send(%Message{payload: payload, event: "event"})

    {:resolve, :ok, state}
  end

  defp handle_wasm({:wasm_call, %{"views" => views}}, state) do
    views =
      views
      |> Enum.map(fn view_string -> "Elixir." <> view_string end)
      |> Enum.map(&String.to_existing_atom/1)
      |> Enum.map(&start_local_live_view/1)
      |> Enum.filter(fn result -> result != nil end)
      |> Map.new()

    {:resolve, :ok, %{state | views: views}}
  end

  defp handle_wasm({:wasm_call, %{"type" => "rerender"}}, state) do
    state.views
    |> Enum.each(fn {name, pid} -> send(pid, %Message{event: "rerender"}) end)

    {:resolve, :ok, state}
  end

  defp start_local_live_view(view) do
    params = %{
      "session" => %Session{view: view}
    }

    ref = make_ref()

    with {:ok, pid} <- LocalLiveView.Server.start_llv_process() do
      send(pid, {LocalLiveView.Server, params, {self(), ref}, %Phoenix.Socket{}})

      receive do
        {^ref, {:ok, _reply}} ->
          {view, pid}

        {^ref, {:error, _reply}} ->
          nil
      end
    end
  end
end
