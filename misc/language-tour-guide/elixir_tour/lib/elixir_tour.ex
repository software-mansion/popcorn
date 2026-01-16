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
    :application.set_env(:elixir, :ansi_enabled, false)
    {:ok, %{}}
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

  defp handle_wasm({:wasm_call, ["eval_elixir", page_id, code]}, all_bindings) do
    bindings = Map.get(all_bindings, page_id, [])

    try do
      case eval(code, bindings) do
        {:ok, result, new_bindings} ->
          all_bindings = Map.put(all_bindings, page_id, new_bindings)
          {:resolve, inspect(result), all_bindings}

        {:error, error_message} ->
          {:reject, error_message, all_bindings}
      end
    rescue
      e ->
        error_message = Exception.format(:error, e)
        {:reject, error_message, all_bindings}
    end
  end

  defp eval(code, bindings) do
    try do
      {evaluated, new_bindings} =
        Code.eval_string(code, bindings, %Macro.Env{
          __ENV__
          | file: "playground",
            line: 1,
            module: nil
        })

      {:ok, evaluated, new_bindings}
    rescue
      e ->
        error_message = Exception.format(:error, e)
        {:error, error_message}
    end
  end
end
