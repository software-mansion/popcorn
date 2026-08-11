defmodule Treeshake.CallGraph do
  @moduledoc false

  # Creates a caller -> callee graph where nodes are only reachable functions (MFAs).

  alias Treeshake.Utils.Graph

  @type t :: Graph.t(mfa())

  @protocol_built_in_types [
    Tuple,
    Atom,
    List,
    BitString,
    Integer,
    Float,
    Function,
    PID,
    Map,
    Port,
    Reference,
    Any
  ]

  @spec create(Treeshake.ModuleIndex.t(), [Treeshake.keep_entry()]) :: t()
  def create(module_index, keep) do
    protocols_impls =
      Enum.flat_map(module_index, fn
        {module, %{protocol_impl: {protocol, type}}} ->
          case module_index[protocol] do
            %{abstraction: {:protocol, _funs}} ->
              [%{protocol: protocol, type: type, impl: module}]

            nil ->
              []

            _non_protocol ->
              raise "Module #{module} implements #{protocol} as if it was a protocol, but it's not"
          end

        _non_protocol ->
          []
      end)

    protocols_impls = %{
      by_protocol: Enum.group_by(protocols_impls, & &1.protocol),
      by_type: Enum.group_by(protocols_impls, & &1.type)
    }

    behaviour_impls =
      module_index
      |> Enum.flat_map(fn {module, %{behaviour_impls: behaviours}} ->
        Enum.map(behaviours, &%{module: module, behaviour: &1})
      end)
      |> Enum.group_by(& &1.behaviour, & &1.module)

    keep =
      keep
      |> Enum.flat_map(fn
        %{behaviour_impls: behaviour} -> Map.get(behaviour_impls, behaviour, [])
        entry -> [entry]
      end)
      |> Enum.flat_map(fn
        {m, f, a} ->
          [{m, f, a}]

        m when is_atom(m) ->
          Map.fetch!(module_index, m).public_functions
          |> Enum.map(fn {{f, a}, _info} -> {m, f, a} end)
      end)

    bfs_acc = %{
      graph: %{},
      protocol_calls: MapSet.new(),
      referenced_modules: MapSet.new(@protocol_built_in_types)
    }

    Graph.bfs(keep, bfs_acc, &visit(&1, &2, module_index, protocols_impls)).graph
  end

  defp visit({m, f, a} = mfa, acc, module_index, protocols_impls) do
    if function_info = get_in(module_index[m].public_functions[{f, a}]) do
      do_visit(mfa, function_info, acc, module_index, protocols_impls)
    else
      # {[], acc}
      do_visit(mfa, %{calls: [], potential_modules: []}, acc, module_index, protocols_impls)
    end
  end

  defp do_visit({_m, f, a} = mfa, function_info, acc, module_index, protocols_impls) do
    %{
      graph: graph,
      protocol_calls: acc_protocol_calls,
      referenced_modules: acc_referenced_modules
    } = acc

    # impl_for references all implementations when protocol is consolidated
    potential_modules =
      if {f, a} in [impl_for: 1, impl_for!: 1], do: [], else: function_info.potential_modules

    referenced_modules_info =
      Enum.flat_map(
        potential_modules,
        &case module_index[&1] do
          nil -> []
          info -> [info]
        end
      )

    acc_referenced_modules =
      MapSet.union(
        acc_referenced_modules,
        MapSet.new(referenced_modules_info, & &1.module)
      )

    behaviour_calls = find_behaviour_calls(referenced_modules_info, module_index)
    child_spec_calls = find_child_spec_calls(referenced_modules_info)

    all_calls =
      (function_info.calls ++ behaviour_calls ++ child_spec_calls)
      |> Enum.reject(&(&1 == mfa))

    {protocol_entries, acc_protocol_calls} =
      find_protocol_calls(
        all_calls,
        acc_protocol_calls,
        acc_referenced_modules,
        referenced_modules_info,
        protocols_impls,
        module_index
      )

    graph =
      graph
      |> merge_graph([{mfa, all_calls}])
      |> merge_graph(protocol_entries)

    acc = %{
      graph: graph,
      protocol_calls: acc_protocol_calls,
      referenced_modules: acc_referenced_modules
    }

    {all_calls ++ Enum.flat_map(protocol_entries, &value/1), acc}
  end

  defp find_behaviour_calls(referenced_modules_info, module_index) do
    referenced_modules_info
    |> Enum.flat_map(fn info ->
      Enum.map(info.behaviour_impls, &{info.module, &1})
    end)
    |> Enum.flat_map(fn {module, behaviour} ->
      case module_index[behaviour] do
        %{abstraction: {:behaviour, callbacks}} ->
          callbacks

        nil ->
          []

        _non_behaviour ->
          raise "Module #{module} implements #{behaviour} as if it was a behaviour, but it's not"
      end
      |> Enum.map(fn {f, a} -> {module, f, a} end)
    end)
  end

  # When a module atom appears as a literal (e.g. passed to Supervisor.start_link),
  # the supervisor will call child_spec/1 on it at runtime — add that edge explicitly.
  defp find_child_spec_calls(referenced_modules_info) do
    referenced_modules_info
    |> Enum.filter(fn info -> Map.has_key?(info.public_functions, {:child_spec, 1}) end)
    |> Enum.map(fn info -> {info.module, :child_spec, 1} end)
  end

  # A protocol callback implementation impl_mod.fun is included when both conditions are met:
  # - impl_mod is referenced
  # - protocol function (protocol.fun) is called
  defp find_protocol_calls(
         all_calls,
         acc_protocol_calls,
         acc_referenced_modules,
         referenced_modules_info,
         protocols_impls,
         module_index
       ) do
    protocol_edges_from_calls =
      all_calls
      |> Enum.filter(fn {cm, cf, ca} ->
        case get_in(module_index[cm].abstraction) do
          {:protocol, funs} -> {cf, ca} in funs
          _other -> false
        end
      end)
      |> Enum.flat_map(fn {cm, cf, ca} ->
        Map.fetch!(protocols_impls.by_protocol, cm)
        |> Enum.filter(&(&1.type in acc_referenced_modules))
        |> Enum.map(&{{&1.protocol, cf, ca}, {&1.impl, cf, ca}})
      end)

    acc_protocol_calls =
      MapSet.union(acc_protocol_calls, MapSet.new(protocol_edges_from_calls, &key/1))

    protocol_edges_from_modules =
      referenced_modules_info
      |> Enum.flat_map(fn info -> Map.get(protocols_impls.by_type, info.module, []) end)
      |> Enum.flat_map(fn %{protocol: protocol, impl: impl} ->
        Map.fetch!(module_index, impl).public_functions
        |> Map.keys()
        |> Enum.filter(fn {f, a} -> {protocol, f, a} in acc_protocol_calls end)
        |> Enum.map(fn {f, a} -> {{protocol, f, a}, {impl, f, a}} end)
      end)

    protocol_entries =
      Enum.group_by(
        protocol_edges_from_modules ++ protocol_edges_from_calls,
        &key/1,
        &value/1
      )

    {protocol_entries, acc_protocol_calls}
  end

  defp merge_graph(graph, to_merge) do
    Enum.reduce(to_merge, graph, fn {k, v}, acc ->
      case Map.fetch(acc, k) do
        {:ok, v2} -> v ++ v2
        :error -> v
      end
      |> Enum.sort()
      |> Enum.dedup()
      |> then(&Map.put(acc, k, &1))
    end)
  end

  defp key({k, _v}), do: k
  defp value({_k, v}), do: v
end
