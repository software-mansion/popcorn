defmodule Local.Rank do
  @moduledoc """
  Fractional-index rank generation for ordering tasks (client-side).

  A rank is a base-36 string interpreted as a fraction `0.d1d2d3…`. `between/2`
  returns a string that sorts lexicographically strictly between its two
  neighbors, so a task can be reordered by writing only its own `position` —
  neighbors never need renumbering. Because the keys are infinitely divisible,
  there is always room to insert between any two of them.

  The task's id (dash-stripped) is appended to the rank so positions stay globally
  unique, which deterministically resolves two simultaneous moves into the same
  slot. `key/3` bundles that, and `key_before/3` builds a position for inserting
  among ordered `%{id, position}` items.
  """

  @digits ~c"0123456789abcdefghijklmnopqrstuvwxyz"
  @base length(@digits)

  @doc """
  A unique rank strictly between `lo` and `hi`, with `id` baked on.

  Either bound may be `nil` (an open end). The dash-stripped `id` suffix keeps two
  equal ranks distinct and deterministically ordered.
  """
  def key(lo, hi, id), do: between(lo, hi) <> id_suffix(id)

  @doc """
  A `key/3` for inserting a task with `id` before `before_id` among `items`.

  `items` is a list of maps with `:id` and `:position` (any order — sorted here by
  `position`). The item with `id` is excluded, so repositioning a task never treats
  its current slot as a neighbor. A `before_id` of `nil`, or one not present,
  appends to the end.
  """
  def key_before(items, before_id, id) do
    sorted = items |> Enum.reject(&(&1.id == id)) |> Enum.sort_by(& &1.position)
    {prev, next} = neighbors(sorted, before_id)
    key(prev, next, id)
  end

  @doc """
  Returns a rank strictly between `lo` and `hi`.

  Either bound may be `nil` (an open end): `nil` `lo` means "before the first",
  `nil` `hi` means "after the last". Assumes `lo < hi` when both are given.

      iex> between(nil, nil)
      "i"
      iex> between("i", "j")
      "ii"
      iex> between("a", "ab")
      "a5"
      iex> between("z", nil)
      "zi"
  """
  def between(lo, hi) do
    to_charlist(lo || "")
    |> between(to_charlist(hi || ""), [])
    |> Enum.reverse()
    |> List.to_string()
  end

  # `lo`/`hi` are charlists of remaining digits; `acc` is the reversed result so far.
  defp between(lo, hi, acc) do
    {l, lo_rest} = take(lo)
    h = if hi == [], do: @base, else: digit(hd(hi))

    cond do
      h - l > 1 ->
        [char(div(l + h, 2)) | acc]

      # Digits adjacent or equal: keep `lo`'s digit and recurse into the
      # remainder. `hi` only stays bounded when its digit matched `l`.
      true ->
        hi_rest = if hi != [] and digit(hd(hi)) == l, do: tl(hi), else: []
        between(lo_rest, hi_rest, [char(l) | acc])
    end
  end

  defp take([]), do: {0, []}
  defp take([d | rest]), do: {digit(d), rest}

  defp digit(char), do: Enum.find_index(@digits, &(&1 == char))
  defp char(value), do: Enum.at(@digits, value)

  # Neighbor positions for inserting before `before_id` within `sorted` items.
  # nil/missing `before_id` means append to the end.
  defp neighbors(sorted, before_id) do
    case before_id && Enum.find_index(sorted, &(&1.id == before_id)) do
      nil -> {last_position(sorted), nil}
      0 -> {nil, hd(sorted).position}
      idx -> {Enum.at(sorted, idx - 1).position, Enum.at(sorted, idx).position}
    end
  end

  defp last_position([]), do: nil
  defp last_position(items), do: List.last(items).position

  defp id_suffix(id), do: String.replace(id, "-", "")
end
