defmodule Treeshake.TableCompactor do
  @moduledoc false
  #   Compacts functions made of many clauses that map integers to constants,
  #   like the Unicode tables of unicode_util, with one clause per code point.
  #   The compiler turns those into a jump table and a sequence of instructions
  #   per clause, which take much more space than the data and compress badly.
  #   They're replaced with a binary search in binaries holding the keys, and
  #   a tuple of the distinct values. The shaker compacts the modules it
  #   rewrites.
  #
  #   A function is compacted when all its clauses but the last map a constant
  #   integer, or a range of them for a single argument, to a constant, and
  #   the last one matches anything. The integers can't be negative.

  alias Treeshake.TableCompactor.Helpers

  # Fewer clauses than that aren't worth it
  @min_clauses 32
  @block_size Helpers.block_size()

  @doc """
  Compacts the tables of a Core Erlang module. Returns the module, and the
  compacted functions.
  """
  def compact(core) do
    defs = :cerl.module_defs(core)

    {defs, compacted} =
      Enum.map_reduce(defs, [], fn {var, fun} = def, compacted ->
        case compact_fun(fun) do
          {:ok, fun} -> {{var, fun}, [:cerl.var_name(var) | compacted]}
          :error -> {def, compacted}
        end
      end)

    case compacted do
      [] ->
        {core, []}

      compacted ->
        # Combining keys is only needed for several arguments
        multiple_args? = Enum.any?(compacted, fn {_name, arity} -> arity > 1 end)
        helpers = Enum.reject(helpers(), &(not multiple_args? and key_helper?(&1)))

        core =
          :cerl.update_c_module(
            core,
            :cerl.module_name(core),
            :cerl.module_exports(core),
            :cerl.module_attrs(core),
            defs ++ helpers
          )

        {core, Enum.reverse(compacted)}
    end
  end

  defp key_helper?({var, _fun}), do: :cerl.var_name(var) == {:popcorn_table_key, 2}

  defp compact_fun(fun) do
    body = :cerl.fun_body(fun)
    args = :cerl.fun_vars(fun)

    with true <- :cerl.is_c_case(body),
         {entries, [catch_all | _] = rest} <- split_table(:cerl.case_clauses(body), args, []),
         true <- length(entries) >= @min_clauses,
         true <- catch_all?(catch_all),
         {:ok, table} <- build_table(entries, length(args)) do
      value = :cerl.c_var(:popcorn@value)

      lookup =
        :cerl.c_apply(:cerl.c_var({:popcorn_table_lookup, 2}), [
          key(args, table),
          :cerl.abstract(table.literal)
        ])

      found = :cerl.c_clause([:cerl.c_tuple([value])], value)

      not_found =
        :cerl.c_clause(
          [:cerl.c_var(:popcorn@none)],
          :cerl.update_c_case(body, :cerl.case_arg(body), rest)
        )

      {:ok, :cerl.update_c_fun(fun, args, :cerl.c_case(lookup, [found, not_found]))}
    else
      _other -> :error
    end
  end

  # The table entries of the leading clauses, and the other clauses
  defp split_table([clause | clauses] = all, args, entries) do
    case table_entry(clause, args) do
      :error -> {Enum.reverse(entries), all}
      entry -> split_table(clauses, args, [entry | entries])
    end
  end

  defp split_table([], _args, entries), do: {Enum.reverse(entries), []}

  defp key([arg], _table), do: arg

  defp key(args, table) do
    :cerl.c_apply(:cerl.c_var({:popcorn_table_key, 2}), [
      :cerl.make_list(args),
      :cerl.abstract(table.arg_widths)
    ])
  end

  # {[key], value} for constant integers, or {{low, high}, value} for a range
  defp table_entry(clause, args) do
    pats = :cerl.clause_pats(clause)
    guard = :cerl.clause_guard(clause)
    body = :cerl.fold_literal(:cerl.clause_body(clause))

    cond do
      not :cerl.is_literal(body) ->
        :error

      Enum.all?(pats, &integer_literal?/1) and true_literal?(guard) ->
        {Enum.map(pats, &:cerl.concrete/1), :cerl.concrete(body)}

      match?([_], pats) and :cerl.is_c_var(hd(pats)) ->
        case range_guard(guard, args) do
          {:ok, range} -> {range, :cerl.concrete(body)}
          :error -> :error
        end

      true ->
        :error
    end
  end

  defp integer_literal?(pat) do
    :cerl.is_literal(pat) and is_integer(:cerl.concrete(pat)) and :cerl.concrete(pat) >= 0
  end

  defp true_literal?(guard), do: :cerl.is_literal(guard) and :cerl.concrete(guard) == true

  # `when is_integer(Arg, Low, High)`, which the compiler wraps in a try
  defp range_guard(guard, [arg]) do
    with true <- :cerl.is_c_try(guard),
         let = :cerl.try_arg(guard),
         true <- :cerl.is_c_let(let),
         call = :cerl.let_arg(let),
         true <- :cerl.is_c_call(call),
         {:erlang, :is_integer} <- call_name(call),
         [var, low, high] <- :cerl.call_args(call),
         true <- :cerl.is_c_var(var) and :cerl.var_name(var) == :cerl.var_name(arg),
         true <- integer_literal?(low) and integer_literal?(high),
         [result] = :cerl.let_vars(let),
         true <- true_test?(:cerl.let_body(let), result),
         true <- false_literal?(:cerl.try_handler(guard)) do
      {:ok, {:cerl.concrete(low), :cerl.concrete(high)}}
    else
      _other -> :error
    end
  end

  defp range_guard(_guard, _args), do: :error

  defp call_name(call) do
    module = :cerl.call_module(call)
    name = :cerl.call_name(call)

    if :cerl.is_literal(module) and :cerl.is_literal(name),
      do: {:cerl.concrete(module), :cerl.concrete(name)},
      else: :error
  end

  # `Result =:= true`
  defp true_test?(body, result) do
    with true <- :cerl.is_c_call(body),
         {:erlang, :"=:="} <- call_name(body),
         [var, literal] <- :cerl.call_args(body) do
      :cerl.is_c_var(var) and :cerl.var_name(var) == :cerl.var_name(result) and
        true_literal?(literal)
    else
      _other -> false
    end
  end

  defp false_literal?(tree), do: :cerl.is_literal(tree) and :cerl.concrete(tree) == false

  defp catch_all?(clause) do
    Enum.all?(:cerl.clause_pats(clause), &:cerl.is_c_var/1) and
      true_literal?(:cerl.clause_guard(clause))
  end

  defp build_table(entries, arity) do
    with {:ok, arg_widths} <- arg_widths(entries, arity) do
      ranges =
        Enum.map(entries, fn
          {{low, high}, value} -> {low, high, value}
          {[key], value} when arity == 1 -> {key, key, value}
          {keys, value} -> {combine(keys, arg_widths), combine(keys, arg_widths), value}
        end)

      {:ok, ranges |> first_matches() |> merge_adjacent() |> encode(arg_widths)}
    end
  end

  # Keys of several arguments are combined, each taking a whole number of
  # bytes
  defp arg_widths(_entries, 1), do: {:ok, nil}

  defp arg_widths(entries, arity) do
    widths =
      entries
      |> Enum.map(fn {keys, _value} -> keys end)
      |> Enum.zip_with(&bytes_width(Enum.max(&1)))

    if length(widths) == arity, do: {:ok, widths}, else: :error
  end

  defp combine(keys, widths) do
    keys
    |> Enum.zip(widths)
    |> Enum.reduce(0, fn {key, width}, acc -> Bitwise.bor(Bitwise.bsl(acc, width), key) end)
  end

  # Disjoint, sorted ranges, with the value of the first clause matching
  # each integer
  defp first_matches(ranges) do
    events =
      ranges
      |> Enum.with_index()
      |> Enum.flat_map(fn {{low, high, value}, clause} ->
        [{low, :start, clause, value}, {high + 1, :stop, clause, value}]
      end)
      |> Enum.group_by(&elem(&1, 0))
      |> Enum.sort()

    {segments, _active, _from} =
      Enum.reduce(events, {[], :gb_sets.empty(), nil}, fn {at, changes},
                                                          {segments, active, from} ->
        segments =
          case :gb_sets.is_empty(active) do
            true -> segments
            false -> [{from, at - 1, active |> :gb_sets.smallest() |> elem(1)} | segments]
          end

        active =
          Enum.reduce(changes, active, fn
            {_at, :start, clause, value}, active -> :gb_sets.add({clause, value}, active)
            {_at, :stop, clause, value}, active -> :gb_sets.delete({clause, value}, active)
          end)

        {segments, active, at}
      end)

    Enum.reverse(segments)
  end

  defp merge_adjacent(ranges) do
    ranges
    |> Enum.chunk_while(
      nil,
      fn
        {low, high, value}, nil ->
          {:cont, {low, high, value}}

        {low, high, value}, {acc_low, acc_high, value} when low == acc_high + 1 ->
          {:cont, {acc_low, high, value}}

        range, acc ->
          {:cont, acc, range}
      end,
      fn acc -> {:cont, acc, nil} end
    )
  end

  defp encode(ranges, arg_widths) do
    values = ranges |> Enum.map(fn {_low, _high, value} -> value end) |> Enum.uniq()
    indexes = values |> Enum.with_index() |> Map.new()

    lows = Enum.map(ranges, fn {low, _high, _value} -> low end)
    blocks = Enum.chunk_every(lows, @block_size)
    block_starts = Enum.map(blocks, &hd/1)

    deltas =
      Enum.flat_map(blocks, fn [_first | _rest] = block ->
        [0 | block |> Enum.chunk_every(2, 1, :discard) |> Enum.map(fn [a, b] -> b - a end)]
      end)

    width = block_starts |> Enum.max() |> bytes_width()
    delta_width = deltas |> Enum.max() |> bytes_width()

    span_width =
      ranges |> Enum.map(fn {low, high, _value} -> high - low end) |> Enum.max() |> bytes_width()

    index_width = bytes_width(length(values) - 1)

    block_starts = for start <- block_starts, into: <<>>, do: <<start::size(width)>>
    deltas = for delta <- deltas, into: <<>>, do: <<delta::size(delta_width)>>
    spans = for {low, high, _value} <- ranges, into: <<>>, do: <<high - low::size(span_width)>>

    value_indexes =
      for {_low, _high, value} <- ranges,
          into: <<>>,
          do: <<Map.fetch!(indexes, value)::size(index_width)>>

    literal =
      {length(ranges), block_starts, deltas, spans, value_indexes, List.to_tuple(values), width,
       delta_width, span_width, index_width}

    %{literal: literal, arg_widths: arg_widths}
  end

  defp bytes_width(0), do: 0

  defp bytes_width(max) do
    bits = max |> Integer.digits(2) |> length()
    div(bits + 7, 8) * 8
  end

  # The functions the compacted functions call, see Helpers
  defp helpers do
    {_module, core} =
      Helpers |> :code.which() |> to_string() |> Treeshake.Utils.BeamReader.read_core!()

    Enum.filter(:cerl.module_defs(core), fn {var, _fun} ->
      {name, _arity} = :cerl.var_name(var)
      String.starts_with?(Atom.to_string(name), "popcorn_table_")
    end)
  end
end
