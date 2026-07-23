defmodule PopdocWasm.IexSession do
  @moduledoc """
  A single IEx-like evaluation session: accumulated binding + env and a
  prompt counter, evaluated with `Code.eval_quoted_with_env/3`. Pure data —
  the owning GenServer (`PopdocWasm`) already serializes access.
  """

  defstruct binding: [], env: nil, counter: 1

  @type t :: %__MODULE__{}

  def new do
    %__MODULE__{env: fresh_env("iex")}
  end

  @doc """
  Evaluates `code` (possibly multi-line) against the session.

  Returns:
    * `{:incomplete, session}` — input ends prematurely (missing `end`,
      unclosed delimiter, dangling operator); nothing evaluated, counter
      unchanged. Detected by `Code.string_to_quoted/1` reporting an empty
      token — the same rule IEx uses; no Regex involved (AtomVM has no :re).
    * `{:ok, inspected, session}` — counter bumped.
    * `{:error, error_map, session}` — counter bumped (errors advance the
      prompt in real IEx too). Parse errors carry an empty stacktrace.
  """
  def eval(%__MODULE__{} = session, code, inspect_opts \\ []) do
    # IEx parses input with its trailing newline; without it, heredoc openers
    # (`@doc \"\"\"`) tokenize as hard errors instead of incomplete input.
    code = ensure_trailing_newline(code)

    case Code.string_to_quoted(code) do
      {:ok, quoted} ->
        case eval_quoted(quoted, session.binding, session.env, inspect_opts) do
          {:ok, result, new_binding, new_env} ->
            new_session = %{
              session
              | binding: new_binding,
                env: new_env,
                counter: session.counter + 1
            }

            {:ok, result, new_session}

          {:error, error_map} ->
            {:error, error_map, %{session | counter: session.counter + 1}}
        end

      {:error, {_meta, _message, ""}} ->
        {:incomplete, session}

      {:error, _} ->
        {:error, parse_error(code), %{session | counter: session.counter + 1}}
    end
  end

  defp ensure_trailing_newline(code) do
    if String.ends_with?(code, "\n"), do: code, else: code <> "\n"
  end

  # Re-parse with string_to_quoted! for the exception's full message
  # (SyntaxError/MismatchedDelimiterError render richer text than the raw
  # error tuple).
  defp parse_error(code) do
    Code.string_to_quoted!(code)
  rescue
    err -> exception_to_error_map(err, "")
  end

  defp exception_to_error_map(err, stacktrace) do
    %{
      kind: :error,
      type: inspect(err.__struct__),
      message: Exception.message(err),
      stacktrace: stacktrace
    }
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

  def format_user_stacktrace(stacktrace) do
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
