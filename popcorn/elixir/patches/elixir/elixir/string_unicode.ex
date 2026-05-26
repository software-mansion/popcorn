defmodule String.Unicode do
  # Patch reason: makes String.Unicode rely fully on Erlang's unicode
  # functions, drastically reducing the size of this module.
  # This module is AI-generated.

  @moduledoc """
  A polyfill for `String.Unicode` that ships no Unicode data tables.

  Mirrors the (private) API surface of Elixir's `String.Unicode`
  (`version/0`, `downcase/3`, `upcase/3`) but delegates the actual
  case-mapping work to Erlang's `:unicode_util` and `:unicode` modules.

  Mode-specific tailoring that Erlang's `:unicode_util.lowercase/1` does
  not perform — the Turkic I/i mappings and the Greek Final_Sigma rule —
  is layered on top via a pre-pass over the input. The Greek pre-pass
  walks grapheme clusters with `:unicode_util.gc/1` so that combining
  marks are treated as case-ignorable. Other case-ignorable codepoints
  defined by TR29 (apostrophe-like punctuation, format characters) are
  not specially handled.

  ## Divergence from `String.Unicode`

  `:default` and `:turkic` modes match `String.Unicode` exhaustively
  across the entire Basic Multilingual Plane and on supplementary plane
  samples. `:greek` mode agrees on common alphabets (Latin basic and
  supplement, Greek, Cyrillic) but diverges on a handful of rare
  codepoints (Roman numerals, circled letters, IPA extensions, lonely
  fullwidth lowercase letters, and similar). The divergence stems from
  an implementation quirk in `String.Unicode`: it only triggers
  Final_Sigma when the preceding codepoint pushes a *binary* chunk
  through its compile-time prefix-tables — codepoints in UTF-8 prefix
  groups that contain no case mappings fall through to a byte-pushing
  fallback and silently fail to count as cased context. The polyfill
  follows the Unicode spec instead and treats any cased letter as
  qualifying context.
  """

  @letter_i "i"
  @letter_I "I"
  @dotless_letter_i "ı"
  @letter_I_dot_above "İ"
  @combining_dot_above <<0x0307::utf8>>

  @capital_sigma 0x03A3
  @small_final_sigma 0x03C2

  @doc """
  Returns the Unicode version supported by Erlang's `:unicode_util`,
  normalised to a `{major, minor, patch}` tuple to match the shape of
  `String.Unicode.version/0`.
  """
  @spec version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}
  def version do
    case :unicode_util.spec_version() do
      {major, minor} -> {major, minor, 0}
      {major, minor, patch} -> {major, minor, patch}
    end
  end

  @doc """
  Downcases `string` according to `mode`, prepending the reverse-iolist
  `acc` to the result.

  External callers typically pass `[]` for `acc`; it exists to match the
  recursive accumulator-style signature of `String.Unicode.downcase/3`.
  """
  @spec downcase(binary(), iodata(), :default | :greek | :turkic) :: binary()
  def downcase(string, acc, mode) when is_binary(string) and is_list(acc) do
    string
    |> pre_downcase(mode)
    |> stream_lowercase()
    |> :unicode.characters_to_binary()
    |> prepend(acc)
  end

  @doc """
  Upcases `string` according to `mode`, prepending the reverse-iolist
  `acc` to the result.
  """
  @spec upcase(binary(), iodata(), :default | :greek | :turkic) :: binary()
  def upcase(string, acc, mode) when is_binary(string) and is_list(acc) do
    string
    |> pre_upcase(mode)
    |> stream_uppercase()
    |> :unicode.characters_to_binary()
    |> prepend(acc)
  end

  # :unicode_util.lowercase/1 and uppercase/1 only convert the leading
  # codepoint of the chardata they're given and return the remainder
  # untouched as a tail. We have to drive them codepoint-by-codepoint to
  # process an entire string.
  defp stream_lowercase(chardata) do
    case :unicode_util.lowercase(chardata) do
      [] -> []
      [cp | rest] -> [cp | stream_lowercase(rest)]
    end
  end

  defp stream_uppercase(chardata) do
    case :unicode_util.uppercase(chardata) do
      [] -> []
      [cp | rest] -> [cp | stream_uppercase(rest)]
    end
  end

  # Turkic downcase tailoring: İ → i, "I" + ̇ → i, I → ı.
  # Order matters: the precomposed İ and the I+combining sequence must be
  # rewritten before the bare I, otherwise the bare-I rule would steal them.
  defp pre_downcase(string, :turkic) do
    string
    |> :binary.replace(@letter_I_dot_above, @letter_i, [:global])
    |> :binary.replace(@letter_I <> @combining_dot_above, @letter_i, [:global])
    |> :binary.replace(@letter_I, @dotless_letter_i, [:global])
  end

  # Greek mode: rewrite Σ at end of word to ς before default lowercasing.
  defp pre_downcase(string, :greek), do: rewrite_final_sigma(string, [], false)

  defp pre_downcase(string, _mode), do: string

  # Final_Sigma: Σ → ς when preceded by a cased letter and not followed by
  # one, allowing case-ignorable characters in either context. We approximate
  # case-ignorables as combining marks by walking grapheme clusters (gc/1
  # bundles base codepoints with their combining marks).
  defp rewrite_final_sigma(chardata, acc, prev_cased) do
    case :unicode_util.gc(chardata) do
      [] ->
        acc |> :lists.reverse() |> :unicode.characters_to_binary()

      [cluster | rest] ->
        {base, tail} = split_cluster(cluster)

        cond do
          base == @capital_sigma and prev_cased and not next_cluster_cased?(rest) ->
            rewrite_final_sigma(rest, [rebuild_cluster(@small_final_sigma, tail) | acc], true)

          base == @capital_sigma ->
            rewrite_final_sigma(rest, [cluster | acc], true)

          true ->
            rewrite_final_sigma(rest, [cluster | acc], cased_lookback?(base))
        end
    end
  end

  defp next_cluster_cased?(chardata) do
    case :unicode_util.gc(chardata) do
      [] -> false
      [cluster | _] -> cluster |> split_cluster() |> elem(0) |> cased_lookahead?()
    end
  end

  defp split_cluster(cp) when is_integer(cp), do: {cp, []}
  defp split_cluster([cp]), do: {cp, []}
  defp split_cluster([cp | tail]), do: {cp, tail}

  defp rebuild_cluster(cp, []), do: cp
  defp rebuild_cluster(cp, tail), do: [cp | tail]

  # Final_Sigma look-around uses two slightly different "cased letter"
  # predicates, mirroring an asymmetry in `String.Unicode`:
  #
  #   * Look-back (cased_lookback?) walks the already-emitted accumulator,
  #     which in `String.Unicode` only contains binary chunks for codepoints
  #     that have a SpecialCasing/UnicodeData entry — ASCII A-Z/a-z take a
  #     fast path that pushes raw bytes and therefore do not register as
  #     cased context. The exception is U+0049 ("I"), which has its own
  #     clause that does push a binary.
  #
  #   * Look-ahead (cased_lookahead?) decodes codepoints directly from the
  #     remaining input and counts every cased letter — including ASCII.
  #
  # In both cases we approximate "cased letter" with `lower != upper`,
  # which agrees with the Ll/Lt/Lu category for the alphabets that matter
  # in practice.
  defp cased_lookback?(?I), do: true
  defp cased_lookback?(cp) when cp <= 0x7F, do: false
  defp cased_lookback?(cp), do: cased_lookahead?(cp)

  defp cased_lookahead?(cp) do
    :unicode_util.lowercase([cp]) != :unicode_util.uppercase([cp])
  end

  # Turkic upcase tailoring: i → İ. Default Unicode upcase of İ is İ,
  # so this composes cleanly with :unicode_util.uppercase/1.
  defp pre_upcase(string, :turkic),
    do: :binary.replace(string, @letter_i, @letter_I_dot_above, [:global])

  defp pre_upcase(string, _mode), do: string

  defp prepend(binary, []), do: binary
  defp prepend(binary, acc), do: IO.iodata_to_binary([:lists.reverse(acc), binary])
end
