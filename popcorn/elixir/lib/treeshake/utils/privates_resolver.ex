defmodule Treeshake.Utils.PrivatesResolver do
  @moduledoc false

  # For each private function Priv and public function Pub that calls Priv (directly or indirectly),
  # moves calls from Priv to Pub (public_functions[Pub].calls) and adds Pub to Priv's calls list
  # (private_functions[Priv]).
  # Mostly vibe-coded.

  alias Treeshake.Utils.Graph
  alias Treeshake.Utils.BeamAnalyzer

  @type public_function_info :: %{calls: [mfa()], potential_modules: [module()]}

  @type name_arity :: {atom(), non_neg_integer()}

  @type module_info :: %{
          module: module(),
          public_functions: %{name_arity() => public_function_info()},
          private_functions: %{name_arity() => [name_arity()]},
          abstraction: {:behaviour | :protocol, [name_arity()]} | nil,
          behaviour_impls: [module()],
          protocol_impl: {module(), module()} | nil
        }

  @spec resolve(BeamAnalyzer.module_info()) :: module_info()
  def resolve(%{module: module, functions: functions} = module_info) do
    {pub_fns, priv_fns} = Enum.split_with(functions, & &1.public)

    priv_index =
      Map.new(priv_fns, fn %{name: name, arity: arity} = fn_info ->
        {{name, arity}, fn_info}
      end)

    # Expand each public function, yielding {pub_key, pub_info, reachable_private_keys}.
    expansions =
      Enum.map(pub_fns, fn pub_fn ->
        reachable = reachable_privates(pub_fn, priv_index)
        all_fns = [pub_fn | Enum.map(reachable, &Map.fetch!(priv_index, &1))]

        pub_info = %{
          calls:
            all_fns
            |> Enum.flat_map(& &1.calls)
            |> Enum.map(&resolve_local(&1, module))
            |> Enum.reject(fn {m, name, arity} ->
              m == module and Map.has_key?(priv_index, {name, arity})
            end)
            |> Enum.uniq(),
          potential_modules: all_fns |> Enum.flat_map(& &1.potential_modules) |> Enum.uniq()
        }

        {{pub_fn.name, pub_fn.arity}, pub_info, reachable}
      end)

    expanded_pub = Map.new(expansions, fn {pub_key, pub_info, _} -> {pub_key, pub_info} end)

    # Build reverse index: private_key -> [public caller keys], preserving pub_fns order.
    priv_caller_map =
      expansions
      |> Enum.flat_map(fn {pub_key, _, reachable} -> Enum.map(reachable, &{&1, pub_key}) end)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))

    expanded_priv =
      Map.new(priv_fns, fn %{name: name, arity: arity} ->
        {{name, arity}, Map.get(priv_caller_map, {name, arity}, [])}
      end)

    %{
      module: module,
      public_functions: expanded_pub,
      private_functions: expanded_priv
    }
    |> Map.merge(Map.take(module_info, [:abstraction, :behaviour_impls, :protocol_impl]))
  end

  defp resolve_local({nil, name, arity}, module), do: {module, name, arity}
  defp resolve_local(call, _module), do: call

  # Returns the MapSet of private function keys transitively reachable from fn_info.
  defp reachable_privates(fn_info, priv_index) do
    seeds = priv_keys_of(fn_info, priv_index)

    Graph.bfs(seeds, MapSet.new(), fn key, acc ->
      neighbors = priv_keys_of(Map.fetch!(priv_index, key), priv_index)
      {neighbors, MapSet.put(acc, key)}
    end)
  end

  defp priv_keys_of(%{calls: calls}, priv_index) do
    for {nil, name, arity} <- calls,
        key = {name, arity},
        Map.has_key?(priv_index, key),
        into: MapSet.new(),
        do: key
  end
end
