defmodule Treeshake.Utils.Graph do
  @moduledoc false

  # Generic graph operations. Mostly vibe-coded.

  @type t :: t(term())
  @type t(graph_node) :: %{graph_node => [graph_node]}

  @spec bfs(Enumerable.t(), acc, (graph_node, acc -> {[graph_node], acc})) :: acc
        when acc: term(), graph_node: term()
  def bfs(seeds, acc, visit) do
    queue = seeds |> Enum.to_list() |> :queue.from_list()
    visited = seeds |> Enum.into(MapSet.new())
    do_bfs(queue, visited, acc, visit)
  end

  defp do_bfs({[], []}, _visited, acc, _visit), do: acc

  defp do_bfs(queue, visited, acc, visit) do
    {{:value, node}, queue} = :queue.out(queue)
    {neighbors, acc} = visit.(node, acc)

    {queue, visited} =
      Enum.reduce(neighbors, {queue, visited}, fn neighbor, {q, vis} ->
        if MapSet.member?(vis, neighbor) do
          {q, vis}
        else
          {:queue.in(neighbor, q), MapSet.put(vis, neighbor)}
        end
      end)

    do_bfs(queue, visited, acc, visit)
  end

  @spec neighborhood(t(graph_node), graph_node, non_neg_integer()) :: t(graph_node)
        when graph_node: term()
  def neighborhood(graph, node, distance) do
    nodes = do_neighborhood(graph, [{node, 0}], MapSet.new([node]), distance)

    Map.new(nodes, fn n ->
      {n, Map.get(graph, n, []) |> Enum.filter(&MapSet.member?(nodes, &1))}
    end)
  end

  defp do_neighborhood(_graph, [], visited, _distance), do: visited

  defp do_neighborhood(graph, [{_node, depth} | queue], visited, distance)
       when depth >= distance do
    do_neighborhood(graph, queue, visited, distance)
  end

  defp do_neighborhood(graph, [{node, depth} | queue], visited, distance) do
    {queue, visited} =
      Enum.reduce(Map.get(graph, node, []), {queue, visited}, fn neighbor, {q, vis} ->
        visit_neighbor(neighbor, depth, q, vis)
      end)

    do_neighborhood(graph, queue, visited, distance)
  end

  defp visit_neighbor(neighbor, depth, queue, visited) do
    if MapSet.member?(visited, neighbor) do
      {queue, visited}
    else
      {queue ++ [{neighbor, depth + 1}], MapSet.put(visited, neighbor)}
    end
  end

  @spec reverse(t()) :: t()
  def reverse(graph) do
    base = Map.new(graph, fn {k, _} -> {k, []} end)

    Enum.reduce(graph, base, fn {from, tos}, acc ->
      Enum.reduce(tos, acc, fn to, acc ->
        Map.update(acc, to, [from], &(&1 ++ [from]))
      end)
    end)
  end

  @spec diff(t(graph_node), t()) :: t(graph_node) when graph_node: term()
  def diff(g1, g2) do
    g1
    |> Enum.flat_map(fn {k, v} ->
      v = v -- Map.get(g2, k, [])
      if v == [], do: [], else: [{k, v}]
    end)
    |> Map.new()
  end

  @spec nodes(t(graph_node)) :: [graph_node] when graph_node: term()
  def nodes(graph) do
    graph |> Enum.flat_map(fn {k, v} -> [k | v] end) |> Enum.sort() |> Enum.dedup()
  end

  @spec to_dot(t()) :: String.t()
  def to_dot(graph) do
    lines = Enum.flat_map(graph, &dot_lines/1)

    """
    digraph {
    #{Enum.map_join(lines, "\n", &"  #{&1}")}
    }
    """
  end

  defp dot_lines({from, []}), do: [inspect(node_label(from))]

  defp dot_lines({from, tos}) do
    from_label = node_label(from)
    Enum.map(tos, fn to -> "#{inspect(from_label)} -> #{inspect(node_label(to))}" end)
  end

  @spec to_mermaid(t()) :: String.t()
  def to_mermaid(graph) do
    ids =
      graph
      |> nodes()
      |> Enum.with_index(fn node, i -> {node, "n#{i}"} end)
      |> Map.new()

    nodes =
      Enum.map(ids, fn {node, id} -> ~s|#{id}["#{node_label(node)}"]| end)

    edges =
      Enum.flat_map(graph, fn
        {from, []} -> [ids[from]]
        {from, tos} -> Enum.map(tos, fn to -> "#{ids[from]} --> #{ids[to]}" end)
      end)

    """
    flowchart TD
    #{Enum.map_join(nodes ++ edges, "\n", &"  #{&1}")}
    """
  end

  defp node_label({m, f, a}) do
    mod = m |> Atom.to_string() |> String.replace_prefix("Elixir.", "")
    "#{mod}.#{f}/#{a}"
  end

  defp node_label(node), do: inspect(node)
end
