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
    {:ok, %{sessions: %{}}}
  end

  @impl true
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, new_state}
  end

  @impl true
  def handle_info(_msg, state), do: {:noreply, state}

  defp handle_wasm({:wasm_call, ["parse_elixir", code, block_id]}, state) do
    case parse(code) do
      {:ok, exprs} ->
        sources = Enum.map(exprs, fn {source, _quoted} -> source end)
        session = %{exprs: exprs, binding: [], env: fresh_env()}
        new_state = put_in(state.sessions[block_id], session)
        {:resolve, %{expressions: sources}, new_state}

      {:error, message} ->
        {:reject, message, state}
    end
  end

  defp handle_wasm({:wasm_call, ["eval_one", block_id, index]}, state) when index >= 0 do
    with {:ok, session} <- Map.fetch(state.sessions, block_id),
         {source, quoted} <- Enum.at(session.exprs, index) do
      case eval_quoted(quoted, session.binding, session.env) do
        {:ok, result, new_binding, new_env} ->
          updated = %{session | binding: new_binding, env: new_env}
          new_state = put_in(state.sessions[block_id], updated)
          {:resolve, %{index: index, code: source, result: result}, new_state}

        {:error, message} ->
          {:resolve, %{index: index, code: source, error: message}, state}
      end
    else
      :error -> {:reject, "no active session for block #{inspect(block_id)}", state}
      nil -> {:reject, "no expression at index #{index}", state}
    end
  end

  defp handle_wasm({:wasm_call, message}, state) do
    {:reject, "unknown wasm call: #{inspect(message)}", state}
  end

  defp handle_wasm({:wasm_cast, _message}, state), do: state

  defp fresh_env do
    %Macro.Env{
      __ENV__
      | file: "playground",
        line: 1,
        module: nil,
        function: nil
    }
  end

  defp parse(code) do
    quoted = Code.string_to_quoted!(code)

    exprs =
      case quoted do
        {:__block__, _, list} -> list
        single -> [single]
      end

    {:ok, Enum.map(exprs, fn expr -> {Macro.to_string(expr), expr} end)}
  rescue
    error -> {:error, Exception.format(:error, error)}
  end

  defp eval_quoted(quoted, binding, env) do
    {value, new_binding, new_env} = Code.eval_quoted_with_env(quoted, binding, env)
    {:ok, inspect(value), new_binding, new_env}
  rescue
    err -> {:error, Exception.format(:error, err)}
  catch
    kind, reason -> {:error, Exception.format(kind, reason, __STACKTRACE__)}
  end
end
