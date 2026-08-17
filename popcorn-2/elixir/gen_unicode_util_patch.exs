# Generates patches/otp/stdlib/unicode_util.erl.
#
# The OTP-generated unicode_util module encodes its Unicode data tables as
# one function clause per codepoint (case_table/1: 2934 clauses, is_extend/1:
# 2527, gc_h_lv_lvt/3: 802). Compiled, those clause piles become ~80KB of
# BEAM jump tables and tuple literals, making unicode_util the largest module
# in popcorn bundles even after function-shaking. This script reads the
# tables back from the stdlib beam's abstract_code chunk and emits a patch
# that stores them as packed binary range tables searched at runtime
# (case_table, is_extend) or replaces them with plain arithmetic
# (gc_h_lv_lvt — Hangul syllables are algorithmic: LV = 44032 + 28k).
#
# The script verifies its own output: the packed lookup is simulated for
# every codepoint in 0..0x10FFFF and compared against the original clause
# data before the patch is written.
#
# Run from popcorn-2/elixir:  elixir gen_unicode_util_patch.exs

defmodule GenUnicodeUtilPatch do
  @out Path.join(__DIR__, "patches/otp/stdlib/unicode_util.erl")
  @case_rec 18
  @ext_rec 6
  @max_cp 0x10FFFF

  def run do
    beam = :code.which(:unicode_util)
    {:ok, {_, [abstract_code: {_, forms}]}} = :beam_lib.chunks(beam, [:abstract_code])

    {case_entries, exceptions} = case_table_entries(forms)
    records = pack_runs(case_entries)
    case_tab = encode_case_records(records)
    verify_case!(case_tab, case_entries, exceptions)

    ext_ranges = is_extend_ranges(forms)
    ext_tab = encode_ext_ranges(ext_ranges)
    verify_ext!(ext_tab, forms)

    verify_hangul!(forms)

    File.write!(@out, render(case_tab, exceptions, ext_tab, records, ext_ranges))

    IO.puts(
      "wrote #{@out}: #{length(records)} case records (#{byte_size(case_tab)}B), " <>
        "#{length(exceptions)} exception clauses, " <>
        "#{length(ext_ranges)} extend ranges (#{byte_size(ext_tab)}B)"
    )
  end

  defp find_fun(forms, name, arity) do
    Enum.find_value(forms, fn
      {:function, _, ^name, ^arity, clauses} -> clauses
      _ -> nil
    end) || raise "function #{name}/#{arity} not found in unicode_util abstract code"
  end

  # ---- case_table/1 ----------------------------------------------------
  # Entries with all-integer mappings are canonicalized to a delta vector
  # (du, dl, dt, df) — 2-tuples {U,L} expand to (U,L,U,L). The consumer
  # (subcat_letter/1) distinguishes 2- from 4-tuples, so shape must be
  # reconstructible: we assert every genuine 4-tuple has dt/=du or df/=dl
  # and derive the shape from the deltas at lookup time. Entries containing
  # multi-codepoint mappings (lists, e.g. uppercase ß -> "SS") stay as
  # literal clauses.

  defp case_table_entries(forms) do
    {entries, exceptions} =
      Enum.reduce(find_fun(forms, :case_table, 1), {[], []}, fn
        {:clause, _, [{:integer, _, cp}], [], [{:tuple, _, elems}]}, {es, exs} ->
          vals = Enum.map(elems, &try_int/1)

          if Enum.all?(vals, & &1) do
            {u, l, t, f} =
              case vals do
                [u, l] -> {u, l, u, l}
                [u, l, t, f] -> {u, l, t, f}
              end

            if length(vals) == 4 and t == u and f == l do
              raise "4-tuple entry #{cp} not distinguishable from 2-tuple shape"
            end

            {[{cp, {u - cp, l - cp, t - cp, f - cp}} | es], exs}
          else
            {es, [{cp, :erl_parse.normalise({:tuple, 0, elems})} | exs]}
          end

        {:clause, _, [{:var, _, _}], [], [body]}, acc ->
          # default clause must be the identity our lookup falls back to
          {:tuple, _, [{:var, _, v}, {:var, _, v}]} = body
          acc
      end)

    {Enum.sort(entries), Enum.sort(exceptions)}
  end

  defp try_int({:integer, _, v}), do: v
  defp try_int(_), do: nil

  # Greedy run packing: stride-1 runs of length >= 3 first; leftovers are
  # split by parity and packed as stride-2 runs (Unicode case blocks
  # commonly alternate Upper/lower on even/odd codepoints).
  defp pack_runs(entries) do
    r1 = runs(entries, 1)
    {multi, single} = Enum.split_with(r1, fn {_, _, _, n} -> n >= 3 end)

    single_entries =
      single
      |> Enum.flat_map(fn {s, e, dv, _} -> Enum.map(s..e, &{&1, dv}) end)
      |> Enum.sort()

    {evens, odds} = Enum.split_with(single_entries, fn {cp, _} -> rem(cp, 2) == 0 end)

    records =
      Enum.map(multi, fn {s, _e, dv, n} -> {s, n, 1, dv} end) ++
        Enum.map(runs(evens, 2) ++ runs(odds, 2), fn {s, _e, dv, n} -> {s, n, 2, dv} end)

    Enum.sort(records)
  end

  defp runs(entries, stride) do
    entries
    |> Enum.reduce([], fn {cp, dv}, acc ->
      case acc do
        [{start, last, ^dv, n} | rest] when cp == last + stride ->
          [{start, cp, dv, n + 1} | rest]

        _ ->
          [{cp, cp, dv, 1} | acc]
      end
    end)
    |> Enum.reverse()
  end

  defp encode_case_records(records) do
    for {start, count, stride, {du, dl, dt, df}} <- records, into: <<>> do
      true = start <= @max_cp and count < 65536 and stride in 1..2
      true = Enum.all?([du, dl, dt, df], &(abs(&1) < 0x800000))
      <<start::24, count::16, stride::8, du::signed-24, dl::signed-24, dt::signed-24, df::signed-24>>
    end
  end

  # Simulates the generated Erlang lookup byte-for-byte for every codepoint
  # and compares with the original clause data.
  defp verify_case!(tab, entries, exceptions) do
    expected = Map.new(entries)
    ex_cps = MapSet.new(exceptions, &elem(&1, 0))
    n = div(byte_size(tab), @case_rec)

    for cp <- 0..@max_cp, not MapSet.member?(ex_cps, cp) do
      got = case_hit(cp, find_le(cp, tab, @case_rec, 0, n - 1, -1), tab, 2)

      want =
        case expected do
          %{^cp => {du, dl, dt, df}} when dt == du and df == dl -> {cp + du, cp + dl}
          %{^cp => {du, dl, dt, df}} -> {cp + du, cp + dl, cp + dt, cp + df}
          _ -> :none
        end

      if got != want, do: raise("case_table mismatch at #{cp}: #{inspect(got)} != #{inspect(want)}")
    end

    :ok
  end

  defp find_le(_cp, _t, _rec, lo, hi, best) when lo > hi, do: best

  defp find_le(cp, t, rec, lo, hi, best) do
    mid = div(lo + hi, 2)
    <<_::binary-size(mid * rec), start::24, _::binary>> = t

    if cp < start,
      do: find_le(cp, t, rec, lo, mid - 1, best),
      else: find_le(cp, t, rec, mid + 1, hi, mid)
  end

  defp case_hit(_cp, _i, _t, 0), do: :none
  defp case_hit(_cp, -1, _t, _tries), do: :none

  defp case_hit(cp, i, t, tries) do
    <<_::binary-size(i * @case_rec), start::24, count::16, stride::8, du::signed-24,
      dl::signed-24, dt::signed-24, df::signed-24, _::binary>> = t

    d = cp - start

    cond do
      rem(d, stride) == 0 and div(d, stride) < count and dt == du and df == dl ->
        {cp + du, cp + dl}

      rem(d, stride) == 0 and div(d, stride) < count ->
        {cp + du, cp + dl, cp + dt, cp + df}

      true ->
        case_hit(cp, i - 1, t, tries - 1)
    end
  end

  # ---- is_extend/1 -----------------------------------------------------

  defp is_extend_ranges(forms) do
    find_fun(forms, :is_extend, 1)
    |> Enum.flat_map(fn
      {:clause, _, [{:integer, _, cp}], [], [{:atom, _, true}]} -> [cp]
      {:clause, _, [{:integer, _, 8205}], [], [{:atom, _, :zwj}]} -> []
      {:clause, _, [{:var, _, _}], [], [{:atom, _, false}]} -> []
    end)
    |> Enum.sort()
    |> Enum.reduce([], fn cp, acc ->
      case acc do
        [{s, e} | rest] when cp == e + 1 -> [{s, cp} | rest]
        _ -> [{cp, cp} | acc]
      end
    end)
    |> Enum.reverse()
  end

  defp encode_ext_ranges(ranges) do
    for {s, e} <- ranges, into: <<>> do
      true = s <= @max_cp and e <= @max_cp
      <<s::24, e::24>>
    end
  end

  defp verify_ext!(tab, forms) do
    truthy =
      find_fun(forms, :is_extend, 1)
      |> Enum.flat_map(fn
        {:clause, _, [{:integer, _, cp}], [], [{:atom, _, true}]} -> [cp]
        _ -> []
      end)
      |> MapSet.new()

    n = div(byte_size(tab), @ext_rec)

    for cp <- 0..@max_cp do
      got =
        case find_le(cp, tab, @ext_rec, 0, n - 1, -1) do
          -1 ->
            false

          i ->
            <<_::binary-size(i * @ext_rec), _s::24, e::24, _::binary>> = tab
            cp <= e
        end

      if got != MapSet.member?(truthy, cp),
        do: raise("is_extend mismatch at #{cp}")
    end

    :ok
  end

  # ---- gc_h_lv_lvt/3 ---------------------------------------------------
  # Asserts the original clause table matches the Hangul-syllable arithmetic
  # the patch relies on, and that the non-table clauses are the expected
  # badarg guard + three defaults (which the patch reproduces verbatim).

  defp verify_hangul!(forms) do
    clauses = find_fun(forms, :gc_h_lv_lvt, 3)

    lv =
      for {:clause, _, [{:cons, _, {:match, _, {:integer, _, cp}, _}, _} | _], [], _} <- clauses,
          do: cp

    lv = Enum.sort(lv)
    expected_lv = for k <- 0..398, do: 44032 + 28 * k
    if lv != expected_lv, do: raise("LV clause set diverged from 44032+28k arithmetic")

    lvt =
      for {:clause, _, _, [guard], body} <- clauses,
          calls_fun?(body, :gc_h_T),
          {lo, hi} <- [guard_range(guard)],
          do: {lo, hi}

    expected_lvt = for s <- expected_lv, do: {s + 1, s + 27}
    if Enum.sort(lvt) != expected_lvt, do: raise("LVT ranges diverged from LV+1..LV+27")

    defaults =
      for {:clause, _, _, [], _} = c <- clauses,
          not match?({:clause, _, [{:cons, _, {:match, _, {:integer, _, _}, _}, _} | _], _, _}, c),
          do: c |> pp_clause() |> String.replace(~r/\s+/, " ")

    expected_defaults = [
      "gc_h_lv_lvt([CP | R1], _, []) -> gc_extend(cp(R1), R1, CP).",
      "gc_h_lv_lvt(R1, R0, [CP]) -> gc_extend(R1, R0, CP).",
      "gc_h_lv_lvt(R1, R0, Acc) -> gc_extend2(R1, R0, Acc)."
    ]

    if defaults != expected_defaults,
      do: raise("gc_h_lv_lvt default clauses changed:\n#{Enum.join(defaults, "\n")}")

    # defaults must come after all table clauses for the patch's clause order
    # to be equivalent
    last_table_idx =
      clauses
      |> Enum.with_index()
      |> Enum.filter(fn {c, _} -> match?({:clause, _, _, [_ | _], _}, c) end)
      |> Enum.map(&elem(&1, 1))
      |> Enum.max()

    if last_table_idx >= length(clauses) - 3,
      do: raise("gc_h_lv_lvt defaults are not the trailing clauses")

    :ok
  end

  defp calls_fun?(body, name) do
    body |> inspect(limit: :infinity) |> String.contains?(":#{name}")
  end

  defp guard_range(conjuncts) do
    lo =
      Enum.find_value(conjuncts, fn
        {:op, _, :"=<", {:integer, _, n}, {:var, _, :CP}} -> n
        _ -> nil
      end)

    hi =
      Enum.find_value(conjuncts, fn
        {:op, _, :"=<", {:var, _, :CP}, {:integer, _, n}} -> n
        _ -> nil
      end)

    if lo && hi, do: {lo, hi}, else: raise("unrecognized gc_h_lv_lvt guard")
  end

  defp pp_clause(clause) do
    {:function, 0, :gc_h_lv_lvt, 3, [clause]}
    |> :erl_pp.form()
    |> IO.iodata_to_binary()
    |> String.trim()
  end

  # ---- emission --------------------------------------------------------

  defp render(case_tab, exceptions, ext_tab, records, ext_ranges) do
    stdlib_vsn = :code.lib_dir(:stdlib) |> Path.basename()

    ex_clauses =
      Enum.map_join(exceptions, "\n", fn {cp, tuple} ->
        "case_ex(#{cp}) -> #{:io_lib.format(~c"~w", [tuple])};"
      end)

    """
    -module(unicode_util).

    %% Generated by gen_unicode_util_patch.exs from #{stdlib_vsn} — DO NOT EDIT.
    %% Regenerate with:  cd popcorn-2/elixir && elixir gen_unicode_util_patch.exs
    %%
    %% Patch reason: OTP generates unicode_util's data tables as one function
    %% clause per codepoint, which compiles to ~80KB of BEAM jump tables and
    %% tuple literals — the largest module in popcorn bundles even after
    %% function-shaking. This patch swaps the three dominant tables for packed
    %% binary range tables + binary search (case_table/1: #{length(records)} records,
    %% is_extend/1: #{length(ext_ranges)} ranges) and Hangul-syllable arithmetic
    %% (gc_h_lv_lvt/3). Multi-codepoint case mappings (#{length(exceptions)} of them, e.g.
    %% uppercase ß) stay as literal clauses. The generator exhaustively diffs
    %% the packed lookups against the original clause data for every codepoint
    %% before writing this file, and asserts the tuple shapes 2- vs 4-tuple are
    %% derivable from the deltas (subcat_letter/1 pattern-matches on them).

    -compile({popcorn_patch_private, case_table/1}).
    -compile({popcorn_patch_private, is_extend/1}).
    -compile({popcorn_patch_private, gc_h_lv_lvt/3}).

    -define(IS_CP(CP), (is_integer(CP) andalso 0 =< CP andalso CP < 16#110000)).

    %% Record: <<Start:24, Count:16, Stride:8, DU:24/signed, DL:24/signed,
    %%           DT:24/signed, DF:24/signed>> — covers Start, Start+Stride, ...
    %% (Count members). Result is CP+delta; 2-tuple shape iff DT==DU, DF==DL.
    %% Stride-2 runs of opposite parity may interleave, so a miss retries the
    %% preceding record once.
    case_table(CP) when is_integer(CP), $a =< CP, CP =< $z -> {CP - 32, CP};
    case_table(CP) when is_integer(CP), $A =< CP, CP =< $Z -> {CP, CP + 32};
    case_table(CP) when is_integer(CP), CP < 128 -> {CP, CP};
    case_table(CP) when is_integer(CP) ->
        case case_ex(CP) of
            none ->
                T = case_tab(),
                case case_hit(CP, find_le(CP, T, 18, 0, byte_size(T) div 18 - 1, -1), T, 2) of
                    none -> {CP, CP};
                    Hit -> Hit
                end;
            Tuple -> Tuple
        end;
    case_table(CP) -> {CP, CP}.

    case_hit(_CP, _I, _T, 0) -> none;
    case_hit(_CP, -1, _T, _Tries) -> none;
    case_hit(CP, I, T, Tries) ->
        Off = I * 18,
        <<_:Off/binary, Start:24, Count:16, Stride:8, DU:24/signed, DL:24/signed,
          DT:24/signed, DF:24/signed, _/binary>> = T,
        D = CP - Start,
        case D rem Stride =:= 0 andalso D div Stride < Count of
            true when DT =:= DU, DF =:= DL -> {CP + DU, CP + DL};
            true -> {CP + DU, CP + DL, CP + DT, CP + DF};
            false -> case_hit(CP, I - 1, T, Tries - 1)
        end.

    %% Index of the last record whose Start =< CP, or -1.
    find_le(_CP, _T, _Rec, Lo, Hi, Best) when Lo > Hi -> Best;
    find_le(CP, T, Rec, Lo, Hi, Best) ->
        Mid = (Lo + Hi) div 2,
        Off = Mid * Rec,
        <<_:Off/binary, Start:24, _/binary>> = T,
        if
            CP < Start -> find_le(CP, T, Rec, Lo, Mid - 1, Best);
            true -> find_le(CP, T, Rec, Mid + 1, Hi, Mid)
        end.

    #{ex_clauses}
    case_ex(_) -> none.

    case_tab() ->
        #{emit_binary(case_tab)}.

    %% Record: <<Start:24, End:24>>, inclusive, sorted, non-overlapping.
    is_extend(8205) -> zwj;
    is_extend(CP) when is_integer(CP) ->
        T = ext_tab(),
        ext_hit(CP, find_le(CP, T, 6, 0, byte_size(T) div 6 - 1, -1), T);
    is_extend(_) -> false.

    ext_hit(_CP, -1, _T) -> false;
    ext_hit(CP, I, T) ->
        Off = I * 6,
        <<_:Off/binary, _Start:24, End:24, _/binary>> = T,
        CP =< End.

    ext_tab() ->
        #{emit_binary(ext_tab)}.

    %% Hangul syllables are algorithmic: the block is 44032 (AC00) to 55203
    %% (D7A3); LV syllables are every 28th codepoint, the rest are LVT.
    gc_h_lv_lvt([CP | _], _R0, _Acc) when not ?IS_CP(CP) ->
        error(badarg);
    gc_h_lv_lvt([CP | R1], _R0, Acc) when 44032 =< CP, CP =< 55203 ->
        case (CP - 44032) rem 28 of
            0 -> popcorn_module:gc_h_V(R1, [CP | Acc]);
            _ -> popcorn_module:gc_h_T(R1, [CP | Acc])
        end;
    gc_h_lv_lvt([CP | R1], _R0, []) ->
        popcorn_module:gc_extend(popcorn_module:cp(R1), R1, CP);
    gc_h_lv_lvt(R1, R0, [CP]) ->
        popcorn_module:gc_extend(R1, R0, CP);
    gc_h_lv_lvt(R1, R0, Acc) ->
        popcorn_module:gc_extend2(R1, R0, Acc).
    """
  end

  defp emit_binary(bin) do
    bytes =
      bin
      |> :binary.bin_to_list()
      |> Enum.chunk_every(24)
      |> Enum.map_join(",\n      ", &Enum.join(&1, ","))

    "<<#{bytes}>>"
  end
end

GenUnicodeUtilPatch.run()
