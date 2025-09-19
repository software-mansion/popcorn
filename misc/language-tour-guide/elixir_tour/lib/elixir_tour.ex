defmodule ElixirTour do
  use GenServer
  import Popcorn.Wasm, only: [is_wasm_message: 1]
  alias Popcorn.Wasm

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl GenServer
  def init(_init_arg) do
    Wasm.register_default_receiver(self(), @process_name)
    {:ok, nil}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, new_state}
  end

  defp handle_wasm({:wasm_call, ["eval_elixir", code]}, state) do
    try do
      result = eval(code)
      {:resolve, inspect(result), state}
    rescue
      e ->
        error_message = "Caught an error: #{inspect(e)}"
        {:reject, error_message, state}
    end
  end

  defp eval(code) do
    try do
      {evaluated, _new_bindings} = Code.eval_string(code, [], __ENV__)
      evaluated
    rescue
      e ->
        error_message = "Caught an error: #{inspect(e)}"
        {:error, error_message}
    end
  end
end
