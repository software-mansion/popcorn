defmodule PopdocWasm do
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
    {:ok, %{bindings: %{}}}
  end

  @impl true
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, new_state}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  defp handle_wasm({:wasm_call, code}, state) when is_binary(code) do
    handle_wasm({:wasm_call, ["eval_elixir", code, "default"]}, state)
  end

  defp handle_wasm({:wasm_call, ["eval_elixir", code]}, state) do
    handle_wasm({:wasm_call, ["eval_elixir", code, "default"]}, state)
  end

  defp handle_wasm({:wasm_call, ["eval_elixir", code, block_id]}, state) do
    bindings = Map.get(state.bindings, block_id, [])

    case eval(code, bindings) do
      {:ok, result, new_bindings} ->
        new_state = put_in(state.bindings[block_id], new_bindings)
        {:resolve, inspect(result), new_state}

      {:error, message} ->
        {:reject, message, state}
    end
  end

  defp handle_wasm({:wasm_call, message}, state) do
    {:reject, "unknown wasm call: #{inspect(message)}", state}
  end

  defp handle_wasm({:wasm_cast, _message}, state), do: state

  defp eval(code, bindings) do
    env = %Macro.Env{
      __ENV__
      | file: "playground",
        line: 1,
        module: nil,
        function: nil
    }

    {result, new_bindings} = Code.eval_string(code, bindings, env)
    {:ok, result, new_bindings}
  rescue
    error -> {:error, Exception.format(:error, error)}
  catch
    kind, reason -> {:error, Exception.format(kind, reason, __STACKTRACE__)}
  end
end
