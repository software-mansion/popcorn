defmodule TestServer do
  use GenServer
  import Popcorn.Wasm, only: [is_wasm_message: 1]
  alias Popcorn.Wasm

  @process_name :main

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: @process_name)
  end

  @impl GenServer
  def init(_args) do
    Wasm.register(@process_name)
    {:ok, nil}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, new_state}
  end

  defp handle_wasm({:wasm_call, %{"action" => "echo", "data" => data}}, state) do
    {:resolve, %{"echoed" => data}, state}
  end

  defp handle_wasm({:wasm_call, %{"action" => "delay", "ms" => ms, "data" => data}}, state) do
    Process.sleep(ms)
    {:resolve, %{"delayed" => data, "ms" => ms}, state}
  end

  defp handle_wasm({:wasm_call, %{"action" => "stdout", "message" => msg}}, state) do
    IO.puts(msg)
    {:resolve, %{"printed" => "stdout"}, state}
  end

  defp handle_wasm({:wasm_call, %{"action" => "crash"}}, _state) do
    raise "intentional crash"
  end

  defp handle_wasm({:wasm_call, data}, state) do
    {:reject, %{"error" => "unknown action", "received" => data}, state}
  end

  defp handle_wasm({:wasm_cast, _data}, state) do
    state
  end
end
