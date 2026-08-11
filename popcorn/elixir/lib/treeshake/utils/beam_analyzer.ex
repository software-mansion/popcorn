defmodule Treeshake.Utils.BeamAnalyzer do
  @moduledoc false
  # Extracts information about functions, behaviours and protocols from core erlang
  # Mostly vibe-coded

  @type function_info :: %{
          name: atom(),
          arity: non_neg_integer(),
          public: boolean(),
          calls: [mfa() | {nil, atom(), non_neg_integer()}],
          potential_modules: [module()]
        }

  @type module_info :: %{
          module: module(),
          functions: [function_info()],
          abstraction:
            {:behaviour | :protocol, [{callback :: atom(), arity :: non_neg_integer()}]} | nil,
          behaviour_impls: [module()],
          protocol_impl: {protocol :: module(), type :: module()} | nil
        }

  @spec analyze(module(), core_ast :: term()) :: module_info()
  def analyze(module, core) do
    exports = collect_exports(core)
    callbacks = collect_callbacks(core)
    protocol_impl = collect_protocol_impl(core)
    behaviours = collect_behaviours(core)

    # Protocol implementations have a :behaviour attr pointing to the protocol,
    # but that is already captured in protocol_impl — exclude it from behaviour_impls.
    behaviours =
      case protocol_impl do
        {protocol, _type} -> List.delete(behaviours, protocol)
        nil -> behaviours
      end

    is_protocol = protocol_definition?(core)
    functions = collect_functions(core, exports)

    abstraction =
      cond do
        is_protocol -> {:protocol, callbacks}
        callbacks != [] -> {:behaviour, callbacks}
        true -> nil
      end

    %{
      module: module,
      functions: functions,
      abstraction: abstraction,
      behaviour_impls: behaviours,
      protocol_impl: protocol_impl
    }
  end

  defp collect_exports({:c_module, _, _, exports, _, _}) do
    exports
    |> Enum.map(fn {:c_var, _, {name, arity}} -> {name, arity} end)
    |> MapSet.new()
  end

  defp collect_callbacks({:c_module, _, _, _, attrs, _}) do
    # Core Erlang groups all attrs with the same key; callbacks value is a list.
    Enum.flat_map(attrs, fn
      {{:c_literal, _, :callback}, {:c_literal, _, cbs}} when is_list(cbs) ->
        for {{name, arity}, _} <- cbs, do: {name, arity}

      _ ->
        []
    end)
  end

  defp collect_behaviours({:c_module, _, _, _, attrs, _}) do
    # Core Erlang groups all behaviour attrs into a single list value.
    Enum.flat_map(attrs, fn
      {{:c_literal, _, :behaviour}, {:c_literal, _, behs}} when is_list(behs) ->
        Enum.filter(behs, &is_atom/1)

      _ ->
        []
    end)
  end

  # Elixir protocol implementations do not use a :protocol_impl attribute.
  # Instead, the compiler generates a __impl__/1 function with two clauses:
  #   def __impl__(:for),      do: ForType
  #   def __impl__(:protocol), do: ProtocolModule
  defp protocol_definition?({:c_module, _, _, _, _, defs}) do
    Enum.any?(defs, fn
      {{:c_var, _, {:__protocol__, 1}}, _} -> true
      _ -> false
    end)
  end

  defp collect_protocol_impl({:c_module, _, _, _, _, defs}) do
    protocol_impl =
      Enum.find(defs, fn
        {{:c_var, _, {:__impl__, 1}}, _} -> true
        _ -> false
      end)

    with {_, {:c_fun, _, _, body}} <- protocol_impl do
      protocol = find_impl_value(body, :protocol)
      for_type = find_impl_value(body, :for)
      if protocol && for_type, do: {protocol, for_type}, else: nil
    end
  end

  defp find_impl_value({:c_case, _, _, clauses}, key) do
    Enum.find_value(clauses, fn
      {:c_clause, _, [{:c_literal, _, ^key}], _, {:c_literal, _, val}} when is_atom(val) -> val
      _ -> nil
    end)
  end

  defp find_impl_value(_, _), do: nil

  defp collect_functions({:c_module, _, _, _, _, defs}, exports) do
    defs
    |> Enum.reject(fn {{:c_var, _, {name, arity}}, _} ->
      name == :module_info and arity in [0, 1]
    end)
    |> Enum.map(fn {{:c_var, _, {name, arity}}, fun_body} ->
      # Letrec-defined helper functions (e.g. "lc$^0") are local to this function
      # body; calls to them should not appear in the call graph (they're not
      # module-level private functions and don't exist as separate graph nodes).
      letrec_names = collect_letrec_names(fun_body)

      {calls, potential_modules} =
        fun_body
        |> collect_calls()
        |> Enum.reject(fn
          {nil, fname, farity} -> MapSet.member?(letrec_names, {fname, farity})
          _ -> false
        end)
        |> Enum.uniq()
        |> Enum.split_with(&is_tuple/1)

      %{
        name: name,
        arity: arity,
        public: MapSet.member?(exports, {name, arity}),
        calls: calls,
        potential_modules: potential_modules
      }
    end)
  end

  # Collects the names of all functions defined by c_letrec nodes within a body.
  defp collect_letrec_names({:c_letrec, _, defs, body}) do
    local =
      Enum.map(defs, fn {{:c_var, _, {name, arity}}, _} -> {name, arity} end)
      |> MapSet.new()

    nested =
      Enum.reduce(defs, MapSet.new(), fn {_, fun_body}, acc ->
        MapSet.union(acc, collect_letrec_names(fun_body))
      end)

    MapSet.union(MapSet.union(local, nested), collect_letrec_names(body))
  end

  defp collect_letrec_names(form) when is_tuple(form) do
    form
    |> Tuple.to_list()
    |> Enum.reduce(MapSet.new(), fn elem, acc -> MapSet.union(acc, collect_letrec_names(elem)) end)
  end

  defp collect_letrec_names([head | tail]) do
    MapSet.union(collect_letrec_names(head), collect_letrec_names(tail))
  end

  defp collect_letrec_names(_), do: MapSet.new()

  # ---- call collection ----

  defp collect_calls([head | tail]), do: collect_calls(head) ++ collect_calls(tail)
  defp collect_calls([]), do: []

  # Functions of the form remote_mod:fun(M, F, Args, ...) where M and F are atom
  # literals and Args is the argument list — extract as a static call to M:F/arity.
  # Covers erlang:spawn/3, erlang:spawn_link/3, erlang:apply/3,
  # erlang:spawn_opt/4, proc_lib:start*/3-5, proc_lib:spawn*/3-5, etc.
  @mfa_callers %{
    erlang: [:spawn, :spawn_link, :apply],
    proc_lib: [:start, :start_link, :start_monitor, :spawn, :spawn_link, :spawn_opt, :spawn_mon]
  }

  defp collect_calls(
         {:c_call, _, {:c_literal, _, mod}, {:c_literal, _, fun},
          [{:c_literal, _, m}, {:c_literal, _, f}, args_expr | rest] = all_args}
       )
       when is_atom(m) and is_atom(f) and is_map_key(@mfa_callers, mod) do
    funs = Map.fetch!(@mfa_callers, mod)

    if fun in funs do
      own =
        case count_list(args_expr) do
          {:ok, arity} -> [{m, f, arity}]
          :error -> []
        end

      [{mod, fun, length(all_args)}] ++ own ++ collect_calls(args_expr) ++ collect_calls(rest)
    else
      [{mod, fun, length(all_args)}] ++ collect_calls(all_args)
    end
  end

  # Remote call: Mod.fun(args)
  defp collect_calls({:c_call, _, {:c_literal, _, mod}, {:c_literal, _, fun}, args})
       when is_atom(mod) and is_atom(fun) do
    [{mod, fun, length(args)}] ++ collect_calls(args)
  end

  # Remote call through variable/expression
  defp collect_calls({:c_call, _, mod_expr, fun_expr, args}) do
    collect_calls(mod_expr) ++ collect_calls(fun_expr) ++ collect_calls(args)
  end

  # Local apply to a known function variable
  defp collect_calls({:c_apply, _, {:c_var, _, {name, arity}}, args})
       when is_atom(name) and is_integer(arity) do
    [{nil, name, arity}] ++ collect_calls(args)
  end

  # Local apply through a variable or expression
  defp collect_calls({:c_apply, _, fun_expr, args}) do
    collect_calls(fun_expr) ++ collect_calls(args)
  end

  # Local function variable reference (higher-order / capture)
  defp collect_calls({:c_var, _, {name, arity}})
       when is_atom(name) and is_integer(arity) do
    [{nil, name, arity}]
  end

  # Hardcoded MFA tuple as a Core Erlang tuple node:
  # {:c_tuple, _, [c_literal(m), c_literal(f), arity_expr]}
  defp collect_calls({:c_tuple, _, [{:c_literal, _, m}, {:c_literal, _, f}, arity_expr]})
       when is_atom(m) and is_atom(f) do
    own =
      case mfa_arity(arity_expr) do
        {:ok, arity} -> [{m, f, arity}]
        :error -> []
      end

    own ++ collect_calls(arity_expr)
  end

  # Hardcoded MFA tuple folded into a c_literal by the compiler:
  # {:c_literal, _, {m, f, arity}} — arity may be an integer or an argument list
  defp collect_calls({:c_literal, _, {m, f, arity}})
       when is_atom(m) and is_atom(f) and is_integer(arity) and arity >= 0 do
    [{m, f, arity}]
  end

  # MFA with argument list rather than integer arity: {m, f, [arg1, ...]}
  defp collect_calls({:c_literal, _, {m, f, args}})
       when is_atom(m) and is_atom(f) and is_list(args) do
    [{m, f, length(args)}]
  end

  # Atom literal — may be a module reference used in dynamic dispatch.
  defp collect_calls({:c_literal, _, v}) when is_atom(v), do: [v]

  # Literal list/map/tuple — collect any atoms nested inside (e.g. supervisor
  # child-spec maps or [HelloPopcorn] passed to Supervisor.start_link/2 get
  # folded into a single c_literal by the compiler, making individual atom
  # nodes invisible to the generic traversal).
  defp collect_calls({:c_literal, _, v}) do
    collect_literal_atoms(v)
  end

  # Case/function clause: only collect from the body, not from patterns or guard.
  # Atoms used as pattern match literals or guard values are not module
  # references and must not pollute potential_modules.
  defp collect_calls({:c_clause, _, _patterns, _guard, body}) do
    collect_calls(body)
  end

  # c_letrec: skip the function variable names in the definitions — they are
  # local definitions, not call sites. Process only the function bodies and the
  # outer continuation body.
  defp collect_calls({:c_letrec, _, defs, body}) do
    Enum.flat_map(defs, fn {_fun_var, fun_body} -> collect_calls(fun_body) end) ++
      collect_calls(body)
  end

  defp collect_calls(form) when is_tuple(form) do
    form |> Tuple.to_list() |> collect_calls()
  end

  defp collect_calls(_), do: []

  defp collect_literal_atoms(atom) when is_atom(atom), do: [atom]

  defp collect_literal_atoms([head | tail]),
    do: collect_literal_atoms(head) ++ collect_literal_atoms(tail)

  defp collect_literal_atoms([]), do: []

  defp collect_literal_atoms(map) when is_map(map) do
    map
    |> Map.to_list()
    |> Enum.flat_map(fn {k, v} -> collect_literal_atoms(k) ++ collect_literal_atoms(v) end)
  end

  # Detect MFA tuples nested inside complex literals (e.g. supervisor child specs
  # folded into a single c_literal by the compiler).
  defp collect_literal_atoms({m, f, arity})
       when is_atom(m) and is_atom(f) and is_integer(arity) and arity >= 0,
       do: [{m, f, arity}]

  defp collect_literal_atoms({m, f, args})
       when is_atom(m) and is_atom(f) and is_list(args),
       do: [{m, f, length(args)}]

  defp collect_literal_atoms(tuple) when is_tuple(tuple) do
    tuple |> Tuple.to_list() |> Enum.flat_map(&collect_literal_atoms/1)
  end

  # External function reference stored as a literal, e.g. &Logger.default_formatter/0
  # compiled as {:c_literal, _, &Logger.default_formatter/0}.
  defp collect_literal_atoms(v) when is_function(v) do
    case :erlang.fun_info(v, :type) do
      {:type, :external} ->
        {:module, m} = :erlang.fun_info(v, :module)
        {:name, f} = :erlang.fun_info(v, :name)
        {:arity, a} = :erlang.fun_info(v, :arity)
        [{m, f, a}]

      _ ->
        []
    end
  end

  defp collect_literal_atoms(_), do: []

  defp mfa_arity({:c_literal, _, a}) when is_integer(a) and a >= 0, do: {:ok, a}

  defp mfa_arity(list_expr) do
    count_list(list_expr)
  end

  defp count_list({:c_literal, _, []}), do: {:ok, 0}
  defp count_list({:c_literal, _, list}) when is_list(list), do: {:ok, length(list)}

  defp count_list({:c_cons, _, _head, tail}) do
    case count_list(tail) do
      {:ok, n} -> {:ok, n + 1}
      :error -> :error
    end
  end

  defp count_list(_), do: :error
end
