defmodule Treeshake.TableCompactorTest do
  use ExUnit.Case, async: true

  alias Treeshake.TableCompactor
  alias Treeshake.TableCompactor.Helpers

  @not_integers [-1, 1.0, 97.0, :a, "a", [97], nil]

  test "unicode_util's tables return the same for every code point" do
    {:unicode_util, core} = Treeshake.Utils.BeamReader.read_core!(:code.which(:unicode_util))
    {compacted_core, compacted} = TableCompactor.compact(core)

    for table <- [unicode_table: 1, case_table: 1, compose_pair: 2, is_extend: 1],
        do: assert(table in compacted)

    original = load(core, :unicode_util_original)
    compacted_module = load(compacted_core, :unicode_util_compacted)
    code_points = Enum.concat([0..0x10FFFF, [0x110000, Integer.pow(2, 40)], @not_integers])

    for {fun, 1} <- compacted, code_point <- code_points do
      assert apply(original, fun, [code_point]) === apply(compacted_module, fun, [code_point])
    end

    # The composed pairs, and their neighbours
    pairs =
      for {base, accent} <- compose_pairs(core),
          base <- [base - 1, base, base + 1],
          accent <- [accent - 1, accent, accent + 1],
          do: [base, accent]

    for [base, accent] <- pairs ++ for(a <- @not_integers, do: [a, 0x301]) do
      assert original.compose_pair(base, accent) ===
               compacted_module.compose_pair(base, accent)
    end
  end

  test "first matching clauses, ranges and fallbacks" do
    source = """
    -module(tables).
    -export([ranges/1, pairs/2, few/1, mixed/1]).

    #{for n <- 1..40, do: "ranges(#{n * 3}) -> {single, #{n}};\n"}
    ranges(X) when is_integer(X, 50, 70) -> range;
    ranges(X) when is_integer(X, 60, 200) -> later_range;
    ranges(X) when is_integer(X, 201, 220) -> range;
    ranges(X) -> {other, X}.

    #{for n <- 1..40, do: "pairs(#{n}, #{n * 300}) -> #{n};\n"}
    pairs(A, B) -> {A, B}.

    #{for n <- 1..31, do: "few(#{n}) -> #{n};\n"}
    few(_) -> none.

    #{for n <- 1..40, do: "mixed(#{n}) -> #{n};\n"}
    mixed(X) when X > 100 -> big;
    mixed(_) -> none.
    """

    core = core_from_source(source)
    {compacted_core, compacted} = TableCompactor.compact(core)

    # Too few clauses for `few`, and `mixed` has a clause that's neither in
    # the table nor matches anything
    assert compacted == [ranges: 1, pairs: 2]

    original = load(core, :tables_original)
    compacted_module = load(compacted_core, :tables_compacted)

    for x <- Enum.concat(-5..300, @not_integers) do
      assert original.ranges(x) === compacted_module.ranges(x)
    end

    for a <- Enum.concat(-1..41, @not_integers),
        b <- [0, 299, 300, 301, 600, 12_000, Integer.pow(2, 30)] do
      assert original.pairs(a, b) === compacted_module.pairs(a, b)
    end
  end

  # They run in modules like unicode_util before Elixir's applications are on
  # the code path
  test "the helpers call nothing but Erlang's BIFs" do
    {_module, core} =
      Helpers |> :code.which() |> to_string() |> Treeshake.Utils.BeamReader.read_core!()

    called_modules =
      for {var, fun} <- :cerl.module_defs(core),
          {name, _arity} = :cerl.var_name(var),
          String.starts_with?(Atom.to_string(name), "popcorn_table_"),
          call <- :cerl_trees.fold(&[&1 | &2], [], fun),
          :cerl.type(call) == :call,
          uniq: true,
          do: :cerl.concrete(:cerl.call_module(call))

    assert called_modules == [:erlang]
  end

  # Loads a module under another name, with all its functions exported
  defp load(core, name) do
    defs = :cerl.module_defs(core)
    exports = Enum.map(defs, fn {var, _fun} -> var end)

    core =
      :cerl.update_c_module(core, :cerl.c_atom(name), exports, :cerl.module_attrs(core), defs)

    {:ok, ^name, beam} = :compile.noenv_forms(core, [:from_core, :binary, :return_errors])
    {:module, ^name} = :code.load_binary(name, ~c"#{name}.beam", beam)
    name
  end

  defp core_from_source(source) do
    {:ok, tokens, _end} = source |> String.to_charlist() |> :erl_scan.string()

    forms =
      tokens
      |> Enum.chunk_by(&match?({:dot, _}, &1))
      |> Enum.chunk_every(2)
      |> Enum.map(fn [form, dot] ->
        {:ok, form} = :erl_parse.parse_form(form ++ dot)
        form
      end)

    {:ok, _module, core} = :compile.noenv_forms(forms, [:to_core, :binary])
    core
  end

  defp compose_pairs(core) do
    [{_var, fun}] =
      Enum.filter(:cerl.module_defs(core), fn {var, _fun} ->
        :cerl.var_name(var) == {:compose_pair, 2}
      end)

    for clause <- fun |> :cerl.fun_body() |> :cerl.case_clauses(),
        [base, accent] = :cerl.clause_pats(clause),
        :cerl.is_literal(base),
        do: {:cerl.concrete(base), :cerl.concrete(accent)}
  end
end
