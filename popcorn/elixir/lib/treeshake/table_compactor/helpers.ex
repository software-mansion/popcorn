defmodule Treeshake.TableCompactor.Helpers do
  @moduledoc false
  #   The functions that TableCompactor adds to the modules it compacts, read
  #   as Core Erlang from this module's debug info. The `popcorn_table_` prefix
  #   keeps them from colliding with the modules' own functions. They end up
  #   in modules like unicode_util, which run before Elixir's applications are
  #   on the code path, so they call nothing but Erlang's BIFs. The entry
  #   points are public, as nothing here calls them.

  import Bitwise

  # The ranges in each block of the binary search
  @block_size 16

  def block_size, do: @block_size

  @doc """
  Finds the value of a key in N sorted, disjoint ranges of integers, or
  returns `:none`. The fields of the ranges are fixed-width integers in
  binaries: their starts' differences from the previous ones, their lengths
  minus one, and the indexes of their values. The starts of each block of
  ranges are in another binary, for a binary search.
  """
  def popcorn_table_lookup(key, {n, block_starts, deltas, spans, indexes, values, w, dw, sw, iw})
      when is_integer(key) and key >= 0 do
    blocks = div(n + @block_size - 1, @block_size)

    case popcorn_table_block(key, block_starts, w, 0, blocks - 1, -1) do
      -1 ->
        :none

      block ->
        block_skip = block * w
        <<_::size(^block_skip)-bits, block_start::size(^w), _::bits>> = block_starts
        first = block * @block_size
        last = min(first + @block_size, n)
        {range, start} = popcorn_table_scan(key, deltas, dw, first + 1, last, first, block_start)
        span_skip = range * sw
        <<_::size(^span_skip)-bits, span::size(^sw), _::bits>> = spans

        if key > start + span do
          :none
        else
          index_skip = range * iw
          <<_::size(^index_skip)-bits, index::size(^iw), _::bits>> = indexes
          {elem(values, index)}
        end
    end
  end

  def popcorn_table_lookup(_key, _table), do: :none

  # The last block starting at or before the key, or -1
  defp popcorn_table_block(_key, _starts, _w, low, high, best) when low > high, do: best

  defp popcorn_table_block(key, starts, w, low, high, best) do
    mid = div(low + high, 2)
    skip = mid * w
    <<_::size(^skip)-bits, start::size(^w), _::bits>> = starts

    if key < start do
      popcorn_table_block(key, starts, w, low, mid - 1, best)
    else
      popcorn_table_block(key, starts, w, mid + 1, high, mid)
    end
  end

  # The last range of a block starting at or before the key, with its start
  defp popcorn_table_scan(key, deltas, dw, range, last, best, best_start) when range < last do
    skip = range * dw
    <<_::size(^skip)-bits, delta::size(^dw), _::bits>> = deltas
    start = best_start + delta

    if start <= key do
      popcorn_table_scan(key, deltas, dw, range + 1, last, range, start)
    else
      {best, best_start}
    end
  end

  defp popcorn_table_scan(_key, _deltas, _dw, _range, _last, best, best_start),
    do: {best, best_start}

  @doc """
  Combines integer arguments into one key, each taking a fixed width, or
  returns `:none`.
  """
  def popcorn_table_key(args, widths), do: popcorn_table_key(args, widths, 0)

  defp popcorn_table_key([arg | args], [w | widths], key)
       when is_integer(arg) and arg >= 0 and arg < 1 <<< w do
    popcorn_table_key(args, widths, key <<< w ||| arg)
  end

  defp popcorn_table_key([], [], key), do: key
  defp popcorn_table_key(_args, _widths, _key), do: :none
end
