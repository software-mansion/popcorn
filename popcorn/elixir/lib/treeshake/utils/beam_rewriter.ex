defmodule Treeshake.Utils.BeamRewriter do
  @moduledoc false

  # Rewrites a core erlang module, keeping only given functions.
  # Mostly vibe-coded.

  @spec keep_funs(Path.t(), [function_arity], [{:stub_removed_public, boolean()}]) ::
          {binary(), [function_arity]}
        when function_arity: {atom(), non_neg_integer()}
  def keep_funs(beam_path, functions, opts \\ []) do
    to_keep = MapSet.new(functions)
    stub_removed_public = Keyword.get(opts, :stub_removed_public, false)

    {module, core} = Treeshake.Utils.BeamReader.read_core!(beam_path)

    {:c_module, anno, name, exports, attrs, defs} = core

    stubbable =
      if stub_removed_public do
        MapSet.new(exports, fn {:c_var, _, {fname, farity}} -> {fname, farity} end)
      else
        MapSet.new()
      end

    {new_defs, removed} =
      Enum.flat_map_reduce(defs, [], fn
        {{:c_var, _, {fname, farity}}, _} = def, acc ->
          cond do
            MapSet.member?(to_keep, {fname, farity}) ->
              {[def], acc}

            MapSet.member?(stubbable, {fname, farity}) ->
              {[make_stub({fname, farity}, module)], [{fname, farity} | acc]}

            true ->
              {[], [{fname, farity} | acc]}
          end
      end)

    new_exports =
      if stub_removed_public do
        exports
      else
        Enum.filter(exports, fn {:c_var, _, {fname, farity}} ->
          MapSet.member?(to_keep, {fname, farity})
        end)
      end

    new_attrs = filter_attrs(attrs, to_keep)

    new_core = {:c_module, anno, name, new_exports, new_attrs, new_defs}

    {compile!(module, new_core, beam_path), removed}
  end

  defp make_stub({fname, farity}, module) do
    vars = for i <- 0..(farity - 1)//1, do: {:c_var, [], :"_V#{i}"}

    body =
      {:c_call, [], {:c_literal, [], :treeshake_helper},
       {:c_literal, [], :raise_treeshaked_error},
       [{:c_literal, [], module}, {:c_literal, [], fname}, {:c_literal, [], farity}]}

    {{:c_var, [], {fname, farity}}, {:c_fun, [], vars, body}}
  end

  defp compile!(module, core, beam_path) do
    # TODO: optionally add no_line_info to save a few bytes from the code chunk
    case :compile.noenv_forms(core, [:from_core, :return_errors, :return_warnings, :debug_info]) do
      {:ok, ^module, binary, _warnings} ->
        binary

      {:ok, other_module, _binary, _warnings} ->
        raise "module name mismatch after recompile: expected #{inspect(module)}, got #{inspect(other_module)} (#{beam_path})"

      {:error, errors, _warnings} ->
        raise "recompile failed for #{beam_path}:\n#{format_errors(errors)}"
    end
  end

  defp filter_attrs(attrs, to_keep) do
    Enum.flat_map(attrs, fn
      {{:c_literal, anno_k, :spec}, {:c_literal, anno_v, specs}} when is_list(specs) ->
        # Core Erlang groups all specs into a single list value.
        filtered =
          Enum.filter(specs, fn
            {{fname, farity}, _} -> MapSet.member?(to_keep, {fname, farity})
            _ -> true
          end)

        case filtered do
          [] -> []
          _ -> [{{:c_literal, anno_k, :spec}, {:c_literal, anno_v, filtered}}]
        end

      {{:c_literal, anno_k, :compile}, {:c_literal, anno_v, value}} ->
        [{{:c_literal, anno_k, :compile}, {:c_literal, anno_v, filter_inline(value, to_keep)}}]

      {{:c_literal, anno_k, :dialyzer}, {:c_literal, anno_v, value}} ->
        case filter_dialyzer(value, to_keep) do
          nil -> []
          filtered -> [{{:c_literal, anno_k, :dialyzer}, {:c_literal, anno_v, filtered}}]
        end

      {{:c_literal, anno_k, :deprecated}, {:c_literal, anno_v, entries}} when is_list(entries) ->
        filtered =
          Enum.filter(entries, fn
            {f, a, _reason} -> MapSet.member?(to_keep, {f, a})
            {f, a} -> MapSet.member?(to_keep, {f, a})
            _ -> true
          end)

        [{{:c_literal, anno_k, :deprecated}, {:c_literal, anno_v, filtered}}]

      attr ->
        [attr]
    end)
  end

  # Returns nil to signal "drop the attribute entirely".
  defp filter_dialyzer(atom, _to_keep) when is_atom(atom), do: atom

  defp filter_dialyzer({type, {f, a}}, to_keep) do
    if MapSet.member?(to_keep, {f, a}), do: {type, {f, a}}, else: nil
  end

  defp filter_dialyzer({type, funs}, to_keep) when is_list(funs) do
    case Enum.filter(funs, fn
           {f, a} -> MapSet.member?(to_keep, {f, a})
           _ -> true
         end) do
      [] -> nil
      filtered -> {type, filtered}
    end
  end

  defp filter_dialyzer(entries, to_keep) when is_list(entries) do
    case Enum.flat_map(entries, &filter_dialyzer_entry(&1, to_keep)) do
      [] -> nil
      filtered -> filtered
    end
  end

  defp filter_dialyzer(other, _to_keep), do: other

  defp filter_dialyzer_entry(entry, to_keep) do
    case filter_dialyzer(entry, to_keep) do
      nil -> []
      v -> [v]
    end
  end

  defp filter_inline({:inline, inlines}, to_keep) when is_list(inlines) do
    {:inline, Enum.filter(inlines, fn {f, a} -> MapSet.member?(to_keep, {f, a}) end)}
  end

  defp filter_inline({:inline, {f, a}}, to_keep) do
    if MapSet.member?(to_keep, {f, a}), do: {:inline, {f, a}}, else: {:inline, []}
  end

  defp filter_inline(opts, to_keep) when is_list(opts) do
    Enum.map(opts, fn
      {:inline, inlines} when is_list(inlines) ->
        {:inline, Enum.filter(inlines, fn {f, a} -> MapSet.member?(to_keep, {f, a}) end)}

      {:inline, {f, a}} ->
        if MapSet.member?(to_keep, {f, a}), do: {:inline, {f, a}}, else: {:inline, []}

      other ->
        other
    end)
  end

  defp filter_inline(other, _to_keep), do: other

  defp format_errors(errors) do
    inspect(errors)
  end
end
