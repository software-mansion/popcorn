defmodule EvalElixir do
  use GenServer

  import Popcorn.Wasm, only: [is_wasm_message: 1]

  alias EvalElixir.Evaluator
  alias Popcorn.Wasm

  @process_name :main

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl GenServer
  def init(_init_arg) do
    Wasm.register(@process_name)
    :application.set_env(:elixir, :ansi_enabled, false)
    {:ok, %{editor_order: [], bindings: %{}}}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end

  defp handle_wasm({:wasm_call, ["eval_elixir", code]}, state) do
    case Evaluator.eval(code) do
      {:ok, result, _bindings} -> {:resolve, inspect(result), state}
      {:error, error_message} -> {:reject, error_message, state}
    end
  end
end
