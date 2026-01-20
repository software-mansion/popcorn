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

    preceding_bindings =
      preceding_editor_ids
      |> Enum.map(&Map.get(bindings_map, &1, []))
      |> Enum.reduce([], &Keyword.merge(&2, &1))

    try do
      case eval(code, preceding_bindings) do
        {:ok, result, new_bindings} ->
          editor_bindings = get_changed(preceding_bindings, new_bindings)
          updated_bindings = Map.put(bindings_map, editor_id, editor_bindings)

          {:resolve, inspect(result), %{state | bindings: updated_bindings}}

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
    Enum.take_while(editor_order, &(&1 != editor_id))
  end

  @spec get_changed(bindings(), bindings()) :: bindings()
  defp get_changed(base_kw, new_kw) do
    unchanged? = fn {key, value} ->
      Keyword.get(base_kw, key) == value
    end

    Enum.reject(new_kw, unchanged?)
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
