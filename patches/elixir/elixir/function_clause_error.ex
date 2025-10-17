defmodule FunctionClauseError do
  @moduledoc """
  An exception raised when a function call doesn't match any defined clause.

  The following fields of this exception are public and can be accessed freely:

    * `:module` (`t:module/0`) - the module name
    * `:function` (`t:atom/0`) - the function name
    * `:arity` (`t:non_neg_integer/0`) - the arity of the function

  For example, if you try to call a function such as `URI.parse/1` with something
  other than a string, the error would look like:

      %FunctionClauseError{
        module: URI,
        function: :parse,
        arity: 1,
        # Other private fields...
      }

  """

  defexception [:module, :function, :arity, :kind, :args, :clauses]

  @clause_limit 10

  @impl true
  def message(exception) do
    case exception do
      %{function: nil} ->
        "no function clause matches"

      %{module: module, function: function, arity: arity} ->
        formatted = Exception.format_mfa(module, function, arity)
        # blamed = blame(exception, &inspect/1, &blame_match/1)
        "no function clause matching in #{formatted}"
    end
  end

  # @impl true
  # def blame(exception, stacktrace) do
  #   {exception, stacktrace}
  # end

  # def blame(_exception, _inspect_fun, _fun) do
  #   ""
  # end

  @impl true
  def blame(%{module: module, function: function, arity: arity} = exception, stacktrace) do
    case stacktrace do
      [{^module, ^function, args, meta} | rest] when length(args) == arity ->
        exception =
          case Exception.blame_mfa(module, function, args) do
            {:ok, kind, clauses} -> %{exception | args: args, kind: kind, clauses: clauses}
            :error -> %{exception | args: args}
          end

        {exception, [{module, function, arity, meta} | rest]}

      stacktrace ->
        {exception, stacktrace}
    end
  end

  defp blame_match(%{match?: true, node: node}), do: Macro.to_string(node)
  defp blame_match(%{match?: false, node: node}), do: "-" <> Macro.to_string(node) <> "-"

  @doc false
  def blame(%{args: nil}, _, _) do
    ""
  end

  def blame(exception, inspect_fun, fun) do
    %{module: module, function: function, arity: arity, kind: kind, args: args, clauses: clauses} =
      exception

    mfa = Exception.format_mfa(module, function, arity)

    format_clause_fun = fn {args, guards} ->
      args = Enum.map_join(args, ", ", fun)
      base = "    #{kind} #{function}(#{args})"
      guards = Enum.to_list(guards)
      Enum.reduce(guards, base, fn g, acc -> "" end) <> "\n"
    end


    # format_clause_fun.([], [])

    "\n\nThe following arguments were given to #{mfa}:\n" <>
      "#{format_args(args, inspect_fun)}" <>
      "#{format_clauses(clauses, format_clause_fun, 10)}"
  end

  defp clause_to_string({op, _, [left, right]} = node, fun, parent) do
    case Code.Identifier.binary_op(op) do
      {_side, precedence} ->
        left = clause_to_string(left, fun, precedence)
        right = clause_to_string(right, fun, precedence)

        if parent > precedence do
          "(" <> left <> " #{op} " <> right <> ")"
        else
          left <> " #{op} " <> right
        end

      _ ->
        fun.(node)
    end
  end

  defp clause_to_string(node, fun, _precedence), do: fun.(node)

  defp format_args(args, inspect_fun) do
    args
    |> Enum.with_index(1)
    |> Enum.map(fn {arg, i} ->
      [pad("\n# "), Integer.to_string(i), pad("\n"), pad(inspect_fun.(arg)), "\n"]
    end)
  end

  defp format_clauses(clauses, format_clause_fun, limit)
  defp format_clauses(nil, _, _), do: ""
  defp format_clauses([], _, _), do: ""
  defp format_clauses(_, _, _), do: ""

  defp format_clauses(clauses, format_clause_fun, limit) do
    top_clauses =
      clauses
      |> Enum.take(limit)
      |> Enum.map(format_clause_fun)

    [
      "\nAttempted function clauses (showing #{length(top_clauses)} out of #{length(clauses)}):",
      "\n\n",
      top_clauses,
      non_visible_clauses(length(clauses) - limit)
    ]
  end

  defp non_visible_clauses(n) when n <= 0, do: []
  defp non_visible_clauses(1), do: ["    ...\n    (1 clause not shown)\n"]
  defp non_visible_clauses(n), do: ["    ...\n    (#{n} clauses not shown)\n"]

  defp pad(string) do
    String.replace(string, "\n", "\n    ")
  end
end
