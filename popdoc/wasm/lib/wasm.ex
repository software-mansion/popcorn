defmodule Popdoc.Wasm do
  use GenServer

  import Popcorn.Wasm, only: [is_wasm_message: 1]
  alias Popcorn.Wasm

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_args) do
    Wasm.ready(@process_name)
    # TODO: needed?
    :application.set_env(:elixir, :ansi_enabled, false)
    {:ok, %{}}
  end

  @impl true
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    next_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, next_state}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  defp handle_wasm({:wasm_call, _message}, state) do
    {:reject, "todo", state}
  end

  defp handle_wasm({:wasm_cast, _message}, state), do: state
end
