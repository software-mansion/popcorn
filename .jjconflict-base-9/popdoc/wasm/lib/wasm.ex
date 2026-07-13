defmodule PopdocWasm do
  use GenServer

  import Popcorn.Wasm, only: [is_wasm_message: 1]
  alias Popcorn.Wasm

  @process_name :main
  @ellipsis_sentinel :__popdoc_ellipsis__
  @snippet_max_len 40

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
        expressions =
          exprs
          |> Enum.with_index()
          |> Enum.map(fn {{source, quoted}, index} ->
            %{
              index: index,
              source: source,
              snippet: snippet(quoted),
              start_line: start_line(quoted)
            }
          end)

        session = %{exprs: exprs, binding: [], env: fresh_env()}
        new_state = put_in(state.sessions[block_id], session)
        {:resolve, %{expressions: expressions}, new_state}

      {:error, message} ->
        {:reject, message, state}
    end
  end

  defp handle_wasm({:wasm_call, ["eval_one", block_id, index]}, state) when index >= 0 do
    with {:ok, session} <- Map.fetch(state.sessions, block_id),
         {_source, quoted} <- Enum.at(session.exprs, index) do
      case eval_quoted(quoted, session.binding, session.env) do
        {:ok, result, new_binding, new_env} ->
          bindings = diff_binding(session.binding, new_binding)
          updated = %{session | binding: new_binding, env: new_env}
          new_state = put_in(state.sessions[block_id], updated)
          {:resolve, %{index: index, result: result, bindings: bindings}, new_state}

        {:error, error_map} ->
          {:resolve, %{index: index, error: error_map}, state}
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

  defp diff_binding(old, new) do
    old_map = Map.new(old)

    for {name, value} <- new, is_atom(name), Map.get(old_map, name) !== value do
      %{name: Atom.to_string(name), value: inspect(value, charlists: :as_lists)}
    end
  end

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
    {:ok, inspect(value, charlists: :as_lists), new_binding, new_env}
  rescue
    err ->
      {:error,
       %{
         kind: :error,
         type: inspect(err.__struct__),
         message: Exception.message(err),
         stacktrace: format_user_stacktrace(__STACKTRACE__)
       }}
  catch
    kind, reason ->
      {:error,
       %{
         kind: kind,
         type: nil,
         message: inspect(reason),
         stacktrace: format_user_stacktrace(__STACKTRACE__)
       }}
  end

  defp format_user_stacktrace(stacktrace) do
    frames =
      stacktrace
      |> Enum.take_while(fn
        {:elixir, :eval_external_handler, _, _} -> false
        _ -> true
      end)
      |> Enum.reject(fn
        {:erlang, :apply, _, _} -> true
        _ -> false
      end)

    if length(frames) >= 2 do
      frames
      |> Exception.format_stacktrace()
      |> String.split("\n")
      |> Enum.map_join("\n", fn
        "    " <> rest -> rest
        line -> line
      end)
    else
      ""
    end
  end

  defp start_line({_, meta, _}) when is_list(meta), do: Keyword.get(meta, :line, 1)
  defp start_line(_), do: 1

  defp snippet(quoted) do
    collapsed = collapse(quoted)

    rendered =
      collapsed
      |> Macro.to_string()
      |> String.replace(inspect(@ellipsis_sentinel), "…")

    truncate(rendered, @snippet_max_len)
  end

  defp collapse(quoted) do
    Macro.prewalk(quoted, fn
      # fn -> body end and other -> arms: collapse the RHS body
      {:->, meta, [lhs, _body]} ->
        {:->, meta, [lhs, @ellipsis_sentinel]}

      # keyword block args: do:/else:/after:/rescue:/catch:
      list when is_list(list) ->
        Enum.map(list, fn
          {key, _val} when key in [:do, :else, :after, :rescue, :catch] ->
            {key, @ellipsis_sentinel}

          other ->
            other
        end)

      other ->
        other
    end)
  end

  defp truncate(string, max) do
    if String.length(string) > max do
      String.slice(string, 0, max - 1) <> "…"
    else
      string
    end
  end
end
