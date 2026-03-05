# Stub: AtomVM fails to percent decode in URI.decode_www_form, so we replace it with a local implementation
defmodule Plug.Conn.Query do
  def decode(query, initial \\ [], invalid_exception \\ Plug.Conn.InvalidQueryError, validate_utf8 \\ true)

  def decode("", initial, _invalid_exception, _validate_utf8) do
    Map.new(initial)
  end

  def decode(query, initial, invalid_exception, validate_utf8) when is_binary(query) do
    parts = :binary.split(query, "&", [:global])

    parts
    |> Enum.reduce(decode_init(), &decode_www_pair(&1, &2, invalid_exception, validate_utf8))
    |> decode_done(initial)
  end

  defp decode_www_pair("", acc, _invalid_exception, _validate_utf8), do: acc

  defp decode_www_pair(binary, acc, invalid_exception, validate_utf8) do
    current =
      case :binary.split(binary, "=") do
        [key, value] ->
          {decode_www_form(key, invalid_exception, validate_utf8),
           decode_www_form(value, invalid_exception, validate_utf8)}
        [key] ->
          {decode_www_form(key, invalid_exception, validate_utf8), ""}
      end

    decode_each(current, acc)
  end

  defp decode_www_form(value, _invalid_exception, _validate_utf8) do
    unpercent(value, "", true)
  end

  # Inlined from Elixir's URI.decode_www_form since AtomVM's version is broken
  defp unpercent(<<?+, tail::binary>>, acc, true) do
    unpercent(tail, <<acc::binary, ?\s>>, true)
  end

  defp unpercent(<<?%, hex1, hex2, tail::binary>>, acc, spaces) do
    dec1 = hex_to_dec(hex1)
    dec2 = hex_to_dec(hex2)

    if is_integer(dec1) and is_integer(dec2) do
      unpercent(tail, <<acc::binary, Bitwise.bsl(dec1, 4) + dec2>>, spaces)
    else
      unpercent(tail, <<acc::binary, ?%>>, spaces)
    end
  end

  defp unpercent(<<head, tail::binary>>, acc, spaces) do
    unpercent(tail, <<acc::binary, head>>, spaces)
  end

  defp unpercent(<<>>, acc, _spaces), do: acc

  defp hex_to_dec(n) when n in ?A..?F, do: n - ?A + 10
  defp hex_to_dec(n) when n in ?a..?f, do: n - ?a + 10
  defp hex_to_dec(n) when n in ?0..?9, do: n - ?0
  defp hex_to_dec(_n), do: nil

  # --- Stateful decoder API (unchanged from Plug) ---

  def decode_init(), do: %{root: []}

  def decode_each({"", value}, map) do
    insert_keys([{:root, ""}], value, map)
  end

  def decode_each({key, value}, map) do
    keys =
      with ?] <- :binary.last(key),
           {pos, 1} when pos > 0 <- :binary.match(key, "[") do
        v = binary_part(key, 0, pos)
        p = pos + 1
        rest = binary_part(key, p, byte_size(key) - p)
        split_keys(rest, key, p, p, v, [{:root, v}])
      else
        _ -> [{:root, key}]
      end

    insert_keys(keys, value, map)
  end

  defp split_keys(<<?], ?[, rest::binary>>, binary, current_pos, start_pos, level, acc) do
    value = split_key(binary, current_pos, start_pos)
    next_level = binary_part(binary, 0, current_pos + 1)
    split_keys(rest, binary, current_pos + 2, current_pos + 2, next_level, [{level, value} | acc])
  end

  defp split_keys(<<?]>>, binary, current_pos, start_pos, level, acc) do
    value = split_key(binary, current_pos, start_pos)
    [{level, value} | acc]
  end

  defp split_keys(<<_, rest::binary>>, binary, current_pos, start_pos, level, acc) do
    split_keys(rest, binary, current_pos + 1, start_pos, level, acc)
  end

  defp split_key(_binary, start, start), do: nil
  defp split_key(binary, current, start), do: binary_part(binary, start, current - start)

  defp insert_keys([{level, key} | rest], value, map) do
    case map do
      %{^level => entries} -> %{map | level => [{key, value} | entries]}
      %{} -> insert_keys(rest, [:pointer | level], Map.put(map, level, [{key, value}]))
    end
  end

  defp insert_keys([], _value, map), do: map

  def decode_done(%{root: root} = decoder, initial \\ []) do
    finalize_map(root, Enum.to_list(initial), decoder)
  end

  defp finalize_pointer(key, map) do
    case Map.fetch!(map, key) do
      [{nil, _} | _] = entries -> finalize_list(entries, [], map)
      entries -> finalize_map(entries, [], map)
    end
  end

  defp finalize_map([{key, [:pointer | pointer]} | rest], acc, map),
    do: finalize_map(rest, [{key, finalize_pointer(pointer, map)} | acc], map)

  defp finalize_map([{nil, _} | rest], acc, map),
    do: finalize_map(rest, acc, map)

  defp finalize_map([{_, _} = kv | rest], acc, map),
    do: finalize_map(rest, [kv | acc], map)

  defp finalize_map([], acc, _map),
    do: Map.new(acc)

  defp finalize_list([{nil, [:pointer | pointer]} | rest], acc, map),
    do: finalize_list(rest, [finalize_pointer(pointer, map) | acc], map)

  defp finalize_list([{nil, value} | rest], acc, map),
    do: finalize_list(rest, [value | acc], map)

  defp finalize_list([{_, _} | rest], acc, map),
    do: finalize_list(rest, acc, map)

  defp finalize_list([], acc, _map),
    do: acc

  # --- Encoder (unchanged from Plug, uses URI.encode_www_form) ---

  def encode(kv, encoder \\ &to_string/1) do
    IO.iodata_to_binary(encode_pair("", kv, encoder))
  end

  defp encode_pair(field, %{__struct__: struct} = map, encoder) when is_atom(struct) do
    [field, ?= | encode_value(map, encoder)]
  end

  defp encode_pair(parent_field, %{} = map, encoder) do
    encode_kv(map, parent_field, encoder)
  end

  defp encode_pair(parent_field, list, encoder) when is_list(list) and is_tuple(hd(list)) do
    encode_kv(Enum.uniq_by(list, &elem(&1, 0)), parent_field, encoder)
  end

  defp encode_pair(parent_field, list, encoder) when is_list(list) do
    mapper = fn
      value when is_map(value) and map_size(value) != 1 ->
        raise ArgumentError,
              "cannot encode maps inside lists when the map has 0 or more than 1 element, " <>
                "got: #{inspect(value)}"
      value ->
        [?&, encode_pair(parent_field <> "[]", value, encoder)]
    end

    list |> Enum.flat_map(mapper) |> prune()
  end

  defp encode_pair(field, nil, _encoder), do: [field, ?=]

  defp encode_pair(field, value, encoder) do
    [field, ?= | encode_value(value, encoder)]
  end

  defp encode_kv(kv, parent_field, encoder) do
    mapper = fn
      {_, value} when value in [%{}, []] -> []
      {field, value} ->
        field =
          if parent_field == "" do
            encode_key(field)
          else
            parent_field <> "[" <> encode_key(field) <> "]"
          end
        [?&, encode_pair(field, value, encoder)]
    end

    kv |> Enum.flat_map(mapper) |> prune()
  end

  defp encode_key(item), do: item |> to_string |> URI.encode_www_form()
  defp encode_value(item, encoder), do: item |> encoder.() |> URI.encode_www_form()

  defp prune([?& | t]), do: t
  defp prune([]), do: []
end
