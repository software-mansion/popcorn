defmodule PopdocWasm.Eval do
  @moduledoc """
  Evaluation helpers shared by eval blocks and the IEx terminal. Stateless:
  callers own their binding + env (`PopdocWasm` keeps them in its GenServer
  state) and thread them through each call.
  """

  @doc """
  Evaluates the string `code` (possibly multi-line) IEx-style.

  Returns:
    * `:incomplete` - input ends mid-expression (missing `end`, unclosed
      delimiter); nothing was evaluated. Same rule IEx uses.
    * `{:ok, inspected, binding, env}`
    * `{:error, error_map}` - the caller keeps its binding and env. Parse
      errors carry an empty stacktrace.
  """
  def eval_string(code, binding, env, inspect_opts \\ []) do
    with {:ok, quoted} <- parse_input(code) do
      eval_quoted(quoted, binding, env, inspect_opts)
    end
  end

  def eval_quoted(quoted, binding, env, inspect_opts \\ []) do
    {value, new_binding, new_env} = Code.eval_quoted_with_env(quoted, binding, env)
    {:ok, inspect(value, [charlists: :as_lists] ++ inspect_opts), new_binding, new_env}
  rescue
    err ->
      {:error, exception_to_error_map(err, format_user_stacktrace(__STACKTRACE__))}
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

  def fresh_env(file) do
    %Macro.Env{
      __ENV__
      | file: file,
        line: 1,
        module: nil,
        function: nil
    }
  end

  defp parse_input(code) do
    {:ok, Code.string_to_quoted!(ensure_trailing_newline(code))}
  rescue
    _ in TokenMissingError -> :incomplete
    err -> {:error, exception_to_error_map(err, "")}
  end

  defp ensure_trailing_newline(code) do
    if String.ends_with?(code, "\n"), do: code, else: code <> "\n"
  end

  defp exception_to_error_map(err, stacktrace) do
    %{
      kind: :error,
      type: inspect(err.__struct__),
      message: Exception.message(err),
      stacktrace: stacktrace
    }
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
end
