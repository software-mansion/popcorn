defmodule Treeshake.Utils.BeamRenamer do
  @moduledoc false

  # Renames a compiled beam file. Mostly vibe-coded.

  @spec rename(binary(), module) :: binary()
  def rename(module, new_name) when is_atom(new_name) do
    {:ok, old_name, chunks} = :beam_lib.all_chunks(module)

    new_chunks =
      Enum.map(chunks, fn
        {~c"AtU8", data} -> {~c"AtU8", replace_atom(data, old_name, new_name)}
        {~c"Atom", data} -> {~c"Atom", replace_atom_legacy(data, old_name, new_name)}
        other -> other
      end)

    build_beam(new_chunks)
  end

  defp replace_atom(<<count::32, rest::binary>>, old, new) do
    {atoms, _} = decode_atoms(rest, count, [])

    new_atoms =
      Enum.map(atoms, fn
        ^old -> new
        other -> other
      end)

    encode_atoms(new_atoms)
  end

  defp decode_atoms(<<>>, 0, acc), do: {Enum.reverse(acc), <<>>}

  defp decode_atoms(<<len, name::binary-size(len), rest::binary>>, n, acc) do
    decode_atoms(rest, n - 1, [String.to_atom(name) | acc])
  end

  defp encode_atoms(atoms) do
    count = length(atoms)

    encoded =
      Enum.map(atoms, fn atom ->
        name = Atom.to_string(atom)
        [byte_size(name), name]
      end)

    IO.iodata_to_binary([<<count::32>>, encoded])
  end

  defp replace_atom_legacy(data, old, new), do: replace_atom(data, old, new)

  defp build_beam(chunks) do
    chunk_data =
      Enum.map(chunks, fn {name, data} ->
        padded = pad4(data)
        [name, <<byte_size(data)::32>>, padded]
      end)

    form_data = IO.iodata_to_binary([~c"BEAM", chunk_data])
    IO.iodata_to_binary([~c"FOR1", <<byte_size(form_data)::32>>, form_data])
  end

  defp pad4(bin) do
    case rem(byte_size(bin), 4) do
      0 -> bin
      n -> [bin, :binary.copy(<<0>>, 4 - n)]
    end
  end
end
