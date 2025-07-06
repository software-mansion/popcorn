defmodule GraphUtils do
  def accessible(graph, node, break_fun \\ fn _node -> false end) do
    {nodes, _visited} = do_accessible(graph, node, MapSet.new(), break_fun)
    nodes
  end

  def do_accessible(graph, node, visited, break_fun) do
    cond do
      MapSet.member?(visited, node) ->
        {[], visited}

      is_nil(graph[node]) ->
        {[], visited}

      true ->
        visited = MapSet.put(visited, node)

        {nodes, visited} =
          graph[node]
          |> Enum.reject(break_fun)
          |> Enum.flat_map_reduce(visited, &do_accessible(graph, &1, &2, break_fun))

        {[node | nodes], visited}
    end
  end
end
