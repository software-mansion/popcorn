defmodule ElixirTour do
  use GenServer
  import Popcorn.Wasm, only: [is_wasm_message: 1]
  alias Popcorn.Wasm

  @process_name :main

  @type editor_id :: String.t()
  @type bindings :: Keyword.t()
  @type state :: %{
          editor_order: [editor_id()],
          bindings: %{editor_id() => bindings()}
        }
  @type wasm_result :: {:resolve, String.t(), state()} | {:reject, String.t(), state()}

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

  @impl GenServer
  def handle_info(_msg, state) do
    # Ignoring unknown message, as it may've been sent
    # by the evaluated code
    {:noreply, state}
  end

  @spec handle_wasm({:wasm_call, list()}, state()) :: wasm_result()
  defp handle_wasm({:wasm_call, ["set_editor_order", editor_order]}, _state) do
    {:resolve, "ok", %{editor_order: editor_order, bindings: %{}}}
  end

  defp handle_wasm({:wasm_call, ["eval_elixir", editor_id, code]}, state) do
    editor_order = Map.get(state, :editor_order, [])
    bindings_map = Map.get(state, :bindings, %{})

    preceding_editor_ids = get_preceding_editors(editor_order, editor_id)

    merged_bindings =
      preceding_editor_ids
      |> Enum.reduce([], fn prev_editor_id, acc ->
        case Map.get(bindings_map, prev_editor_id) do
          bindings when is_list(bindings) ->
            Keyword.merge(acc, bindings)

          nil ->
            acc
        end
      end)

    try do
      case eval(code, merged_bindings) do
        {:ok, result, new_bindings} ->
          editor_bindings = extract_new_bindings(merged_bindings, new_bindings)
          updated_bindings = Map.put(bindings_map, editor_id, editor_bindings)
          updated_state = %{state | bindings: updated_bindings}

          {:resolve, inspect(result), updated_state}

        {:error, error_message} ->
          {:reject, error_message, state}
      end
    rescue
      e ->
        error_message = Exception.format(:error, e)

        {:reject, error_message, state}
    end
  end

  @spec get_preceding_editors([editor_id()], editor_id()) :: [editor_id()]
  defp get_preceding_editors(editor_order, editor_id) do
    case Enum.find_index(editor_order, &(&1 == editor_id)) do
      nil -> []
      index -> Enum.take(editor_order, index)
    end
  end

  @spec extract_new_bindings(bindings(), bindings()) :: bindings()
  defp extract_new_bindings(input_bindings, output_bindings) do
    output_bindings
    |> Enum.filter(fn {key, value} ->
      case Keyword.get(input_bindings, key) do
        old_value when old_value !== value -> true
        _ -> false
      end
    end)
  end

  @spec eval(String.t(), bindings()) :: {:ok, any(), bindings()} | {:error, String.t()}
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
