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
    Wasm.register(@process_name)
    {:ok, nil}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end

  @impl GenServer
  def handle_info(_msg, state) do
    # Ignoring unknown message, as it may've been sent
    # by the evaluated code
    {:noreply, state}
  end

  defp handle_wasm({:wasm_call, ["eval_elixir", code]}, state) do
    try do
      case eval(code) do
        {:ok, result} ->
          {:resolve, inspect(result), state}

        {:error, error_message} ->
          {:reject, error_message, state}
      end
    rescue
      e ->
        error_message = Exception.format(:error, e)
        {:reject, error_message, state}
    end
  end

  defp eval(code) do
    try do
      {evaluated, _new_bindings} = Code.eval_string(code, [], __ENV__)
      {:ok, evaluated}
    rescue
      e ->
        error_message = Exception.format(:error, e)
        {:error, error_message}
    end
  end
end
