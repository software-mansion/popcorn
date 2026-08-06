defmodule ElixirTour do
  use GenServer

  alias ElixirTour.Evaluator

  @process_name :main

  @type editor_id :: String.t()
  @type state :: %{
          editor_order: [editor_id()],
          bindings: %{editor_id() => Evaluator.bindings()}
        }
  @type wasm_result :: {:resolve, String.t(), state()} | {:reject, String.t(), state()}

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl GenServer
  def init(_init_arg) do
    :application.set_env(:elixir, :ansi_enabled, false)
    {:ok, %{editor_order: [], bindings: %{}}}
  end

  @impl GenServer
  def handle_call(["eval_elixir", editor_id, code, editor_order], _from, state) do
    {reply, state} = eval(editor_id, code, editor_order, state)
    {:reply, reply, state}
  end

  @impl GenServer
  def handle_info(_msg, state) do
    # Ignoring unknown message, as it may've been sent
    # by the evaluated code
    {:noreply, state}
  end

  defp eval(editor_id, code, editor_order, state) do
    %{bindings: bindings_map} = state

    preceding_editor_ids = get_preceding_editors(state.editor_order, editor_id)

    preceding_bindings =
      preceding_editor_ids
      |> Enum.map(&Map.get(bindings_map, &1, []))
      |> Enum.reduce([], &Keyword.merge(&2, &1))

    case Evaluator.eval(code, preceding_bindings) do
      {:ok, result, new_bindings} ->
        editor_bindings = get_changed(preceding_bindings, new_bindings)
        updated_bindings = Map.put(bindings_map, editor_id, editor_bindings)

        {%{data: inspect(result)},
         %{state | editor_order: editor_order, bindings: updated_bindings}}

      {:error, error_message} ->
        {%{error: error_message}, state}
    end
  end

  @spec get_preceding_editors([editor_id()], editor_id()) :: [editor_id()]
  defp get_preceding_editors(editor_order, editor_id) do
    Enum.take_while(editor_order, &(&1 != editor_id))
  end

  @spec get_changed(Evaluator.bindings(), Evaluator.bindings()) :: Evaluator.bindings()
  defp get_changed(base_kw, new_kw) do
    unchanged? = fn {key, value} ->
      Keyword.get(base_kw, key) == value
    end

    Enum.reject(new_kw, unchanged?)
  end
end
