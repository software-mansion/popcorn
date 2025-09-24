defmodule LocalLiveView.Dispatcher do
  @moduledoc false
  use GenServer
  alias Phoenix.LiveView.Session
  alias Phoenix.LiveView.Diff
  import Popcorn.Wasm
  alias Popcorn.Wasm
  alias Phoenix.LiveView.Static
  alias LocalLiveView.Message
  @process_name :main

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

  def handle_info(any, state) do
    {:noreply, state}
  end

  defp handle_wasm(
         {:wasm_call, %{"event" => type, "view" => view_string, "payload" => payload}},
         state
       ) do
    view = String.to_existing_atom("Elixir." <> view_string)
    payload = payload |> Map.merge(%{"type" => %{}, "value" => %{}})

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

  def start_local_live_view(view) do
    params = %{
      "session" => %Session{view: view}
    }

    {:ok, pid} =
      DynamicSupervisor.start_child(
        LocalLiveView.Server.Supervisor,
        LocalLiveView.Server.child_spec([])
      )

    ref = make_ref()

    with {:ok, pid} <- start_llv_process() do
      send(pid, {LocalLiveView.Server, params, {self(), ref}, %Phoenix.Socket{}})

      receive do
        {^ref, {:ok, reply}} ->
          reply
          |> Diff.to_iodata()
          |> diff_iodata_to_binary()
          |> LocalLiveView.JS.rerender(view)

          {view, pid}

        {^ref, {:error, reply}} ->
          nil
      end
    end
  end

  def start_llv_process() do
    DynamicSupervisor.start_child(
      LocalLiveView.Server.Supervisor,
      LocalLiveView.Server.child_spec([])
    )
  end

  def diff_iodata_to_binary(list_of_binaries) when is_list(list_of_binaries) do
    Enum.reduce(list_of_binaries, "", fn
      integer, acc when is_integer(integer) -> acc <> List.to_string([integer])
      list, acc when is_list(list) -> acc <> diff_iodata_to_binary(list)
      binary, acc -> acc <> to_string(binary)
    end)
  end

  def render(assigns) do
    :ok
  end
end
