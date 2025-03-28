#
# This file is part of AtomVM.
#
# Copyright 2020 Davide Bettio <davide@uninstall.it>
# Copyright 2012-2022 Elixir Contributors
# https://github.com/elixir-lang/elixir/commits/main/lib/elixir/lib/kernel.ex
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

defmodule Kernel do
  # This avoids crashing the compiler at build time
  @compile {:autoload, false}

  @doc """
  Returns an integer which is the arithmetical absolute value of `number`.

  ## Examples
    iex> abs(3)
    3
    iex> abs(-3)
    3
  """
  def abs(number), do: :erlang.abs(number)

  def div(dividend, divisor), do: :erlang.div(dividend, divisor)

  def rem(dividend, divisor), do: :erlang.rem(dividend, divisor)

  @doc """
  Returns the biggest of the two given terms according to
  Erlang's term ordering.

  If the terms compare equal, the first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> max(1, 2)
      2
      iex> max(:a, :b)
      :b

  Using Erlang's term ordering means that comparisons are
  structural and not semantic. For example, when comparing dates:

      iex> max(~D[2017-03-31], ~D[2017-04-01])
      ~D[2017-03-31]

  In the example above, `max/1` returned March 31st instead of April 1st
  because the structural comparison compares the day before the year. In
  such cases it is common for modules to provide functions such as
  `Date.compare/2` that perform semantic comparison.
  """
  @spec max(first, second) :: first | second when first: term, second: term
  def max(first, second) do
    :erlang.max(first, second)
  end

  @doc """
  Returns the smallest of the two given terms according to
  Erlang's term ordering.

  If the terms compare equal, the first one is returned.

  Inlined by the compiler.

  ## Examples

      iex> min(1, 2)
      1
      iex> min("foo", "bar")
      "bar"

  Using Erlang's term ordering means that comparisons are
  structural and not semantic. For example, when comparing dates:

      iex> min(~D[2017-03-31], ~D[2017-04-01])
      ~D[2017-04-01]

  In the example above, `min/1` returned April 1st instead of March 31st
  because the structural comparison compares the day before the year. In
  such cases it is common for modules to provide functions such as
  `Date.compare/2` that perform semantic comparison.
  """
  @spec min(first, second) :: first | second when first: term, second: term
  def min(first, second) do
    :erlang.min(first, second)
  end

  # Taken from Elixir kernel.ex
  @doc """
  Similar to `struct/2` but checks for key validity.

  The function `struct!/2` emulates the compile time behaviour
  of structs. This means that:

    * when building a struct, as in `struct!(SomeStruct, key: :value)`,
      it is equivalent to `%SomeStruct{key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      If the struct is enforcing any key via `@enforce_keys`, those will
      be enforced as well;

    * when updating a struct, as in `struct!(%SomeStruct{}, key: :value)`,
      it is equivalent to `%SomeStruct{struct | key: :value}` and therefore this
      function will check if every given key-value belongs to the struct.
      However, updating structs does not enforce keys, as keys are enforced
      only when building;

  """
  @spec struct!(module | struct, Enum.t()) :: struct | no_return
  def struct!(struct, fields \\ [])

  def struct!(struct, fields) when is_atom(struct) do
    struct.__struct__(fields)
  end

  def struct!(struct, fields) when is_map(struct) do
    struct(struct, fields, fn
      {:__struct__, _}, acc ->
        acc

      {key, val}, acc ->
        Map.replace!(acc, key, val)
    end)
  end

  defp struct(struct, [], _fun) when is_atom(struct) do
    struct.__struct__()
  end

  defp struct(struct, fields, fun) when is_atom(struct) do
    struct(struct.__struct__(), fields, fun)
  end

  defp struct(%_{} = struct, [], _fun) do
    struct
  end

  defp struct(%_{} = struct, fields, fun) do
    Enum.reduce(fields, struct, fun)
  end

# Patch reason: the "left in right" macro is not capable of
# being used in guard if it is defined outside of Kernel.ex patch

  @doc guard: true
  defmacro left in right do
    in_body? = __CALLER__.context == nil

    expand =
      case :flb_module.bootstrapped?(Macro) do
        true -> &Macro.expand(&1, __CALLER__)
        false -> & &1
      end

    case expand.(right) do
      [] when not in_body? ->
        false

      [] ->
        quote do
          _ = unquote(left)
          false
        end

      [head | tail] = list ->
        # We only expand lists in the body if they are relatively
        # short and it is made only of literal expressions.
        case not in_body? or :flb_module.small_literal_list?(right) do
          true -> :flb_module.in_var(in_body?, left, &:flb_module.in_list(&1, head, tail, expand, list, in_body?))
          false -> quote(do: :lists.member(unquote(left), unquote(right)))
        end

      %{} = right ->
        raise ArgumentError, "found unescaped value on the right side of in/2: #{inspect(right)}"

      right ->
        with {:%{}, _meta, fields} <- right,
             [__struct__: Elixir.Range, first: first, last: last, step: step] <-
               :lists.usort(fields) do
          :flb_module.in_var(in_body?, left, &:flb_module.in_range(&1, expand.(first), expand.(last), expand.(step)))
        else
          _ when in_body? ->
            quote(do: Elixir.Enum.member?(unquote(right), unquote(left)))

          _ ->
            :flb_module.raise_on_invalid_args_in_2(right)
        end
    end
  end

  # Patch reason: the Kernel.to_string macro is not seen as a macro
  # unless it is implemented inside a patch

  defmacro to_string(term) do
    quote(do: :"Elixir.String.Chars".to_string(unquote(term)))
  end

end
