defmodule ErlAST do
  require Logger

  def parse(path) do
    {:ok, ast} = :epp_dodger.quick_parse_file(~c"#{path}")
    ast
  end

  def analyze(ast) do
    module =
      Enum.find_value(ast, fn
        {:attribute, _line, :module, {module, _opts}} -> module
        {:attribute, _line, :module, module} -> module
        _other -> false
      end)

    functions =
      Enum.flat_map(ast, fn
        {:function, _line, function, arity, ast} -> [{function, arity, ast}]
        _other -> []
      end)

    imports =
      Enum.flat_map(ast, fn
        {:attribute, _line, :import, {module, functions}} -> Enum.map(functions, &{&1, module})
        _other -> []
      end)
      |> Enum.concat(Enum.map(functions, fn {f, a, _ast} -> {{f, a}, module} end))
      |> Map.new()

    functions =
      Enum.map(functions, fn {function, arity, ast} ->
        mfa = {module, function, arity}
        {mfa, analyze_function(ast, module, function, arity, imports)}
      end)

    {module, functions}
  end

  defp analyze_function(ast, module, function, arity, imports) do
    ast
    |> find_calls()
    |> Enum.uniq()
    |> Enum.flat_map(fn
      {^module, ^function, ^arity} ->
        []

      {nil, ^function, ^arity} ->
        []

      {nil, function, arity} when is_map_key(imports, {function, arity}) ->
        module = Map.fetch!(imports, {function, arity})
        [{module, function, arity}]

      {nil, function, arity} ->
        [{:erlang, function, arity}]

      other ->
        [other]
    end)
  end

  defp find_calls({:call, _, {:atom, _, function}, args}) do
    [{nil, function, length(args)}]
  end

  defp find_calls({:call, _, {:remote, _, {:atom, _, module}, {:atom, _, function}}, args}) do
    [{module, function, length(args)}]
  end

  defp find_calls({:call, _line, _call, _args}) do
    [{:DYNAMIC_CALL, :DYNAMIC_CALL, 0}]
  end

  defp find_calls(tuple) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> Enum.flat_map(&find_calls/1)
  end

  defp find_calls(list) when is_list(list) do
    Enum.flat_map(list, &find_calls/1)
  end

  defp find_calls(_other) do
    []
  end
end
