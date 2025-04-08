#
# This file is part of AtomVM.
#
# Copyright 2021 Davide Bettio <davide@uninstall.it>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# SPDX-License-Identifier: Apache-2.0 OR LGPL-2.1-or-later
#

defmodule Module do
  @compile {:autoload, false}

  def concat(args) when is_list(args) do
    {parts, acc} = ensure_starts_with_elixir(args)

    do_concat(parts, acc)
    |> :erlang.binary_to_atom(:latin1)
  end

  def concat(a, b) when (is_atom(a) or is_binary(a)) and (is_atom(b) or is_binary(b)) do
    # a_string = :erlang.atom_to_binary(a, :latin1)
    # <<"Elixir.", b_string::binary>> = :erlang.atom_to_binary(b, :latin1)
    #
    # :erlang.binary_to_atom(a_string <> "." <> b_string, :latin1)

    concat([a, b])
  end

  defp ensure_starts_with_elixir([head | tail]) when is_atom(head) and not is_nil(head) do
    ensure_starts_with_elixir([:erlang.atom_to_binary(head, :latin1) | tail])
  end

  defp ensure_starts_with_elixir(["Elixir." <> _ = head | tail]), do: {tail, head}
  defp ensure_starts_with_elixir(["Elixir" = head | tail]), do: {tail, head}
  defp ensure_starts_with_elixir(args), do: {args, "Elixir"}

  defp do_concat([nil | tail], acc), do: do_concat(tail, acc)

  defp do_concat([head | tail], acc) when is_atom(head) do
    bin_head = head |> :erlang.atom_to_binary(:latin1) |> to_partial()
    do_concat(tail, acc <> "." <> bin_head)
  end

  defp do_concat([head | tail], acc) when is_binary(head) do
    do_concat(tail, acc <> "." <> to_partial(head))
  end

  defp do_concat([], acc), do: acc

  defp to_partial("Elixir." <> arg), do: arg
  defp to_partial("." <> arg), do: arg
  defp to_partial(arg) when is_binary(arg), do: arg
end
