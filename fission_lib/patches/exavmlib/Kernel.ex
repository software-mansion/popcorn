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
  Creates and updates structs.

  The `struct` argument may be an atom (which defines `defstruct`)
  or a `struct` itself. The second argument is any `Enumerable` that
  emits two-element tuples (key-value pairs) during enumeration.

  Keys in the `Enumerable` that don't exist in the struct are automatically
  discarded. Note that keys must be atoms, as only atoms are allowed when
  defining a struct.

  This function is useful for dynamically creating and updating structs, as
  well as for converting maps to structs; in the latter case, just inserting
  the appropriate `:__struct__` field into the map may not be enough and
  `struct/2` should be used instead.

  ## Examples

      defmodule User do
        defstruct name: "john"
      end

      struct(User)
      #=> %User{name: "john"}

      opts = [name: "meg"]
      user = struct(User, opts)
      #=> %User{name: "meg"}

      struct(user, unknown: "value")
      #=> %User{name: "meg"}

      struct(User, %{name: "meg"})
      #=> %User{name: "meg"}

      # String keys are ignored
      struct(User, %{"name" => "meg"})
      #=> %User{name: "john"}

  """
  @spec struct(module | struct, Enum.t()) :: struct
  def struct(struct, fields \\ []) do
    struct(struct, fields, fn
      {:__struct__, _val}, acc ->
        acc

      {key, val}, acc ->
        case acc do
          %{^key => _} -> %{acc | key => val}
          _ -> acc
        end
    end)
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

  @doc guard: true
  defmacro left in right do
    in_body? = __CALLER__.context == nil

    expand =
      case bootstrapped?(Macro) do
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
        case not in_body? or small_literal_list?(right) do
          true -> in_var(in_body?, left, &in_list(&1, head, tail, expand, list, in_body?))
          false -> quote(do: :lists.member(unquote(left), unquote(right)))
        end

      %{} = right ->
        raise ArgumentError, "found unescaped value on the right side of in/2: #{inspect(right)}"

      right ->
        with {:%{}, _meta, fields} <- right,
             [__struct__: Elixir.Range, first: first, last: last, step: step] <-
               :lists.usort(fields) do
          in_var(in_body?, left, &in_range(&1, expand.(first), expand.(last), expand.(step)))
        else
          _ when in_body? ->
            quote(do: Elixir.Enum.member?(unquote(right), unquote(left)))

          _ ->
            raise_on_invalid_args_in_2(right)
        end
    end
  end

  defp raise_on_invalid_args_in_2(right) do
    raise ArgumentError, <<
      "invalid right argument for operator \"in\", it expects a compile-time proper list ",
      "or compile-time range on the right side when used in guard expressions, got: ",
      Macro.to_string(right)::binary
    >>
  end

  defp in_var(false, ast, fun), do: fun.(ast)

  defp in_var(true, {atom, _, context} = var, fun) when is_atom(atom) and is_atom(context),
       do: fun.(var)

  defp in_var(true, ast, fun) do
    quote do
      var = unquote(ast)
      unquote(fun.(quote(do: var)))
    end
  end

  defp small_literal_list?(list) when is_list(list) and length(list) <= 32 do
    :lists.all(fn x -> is_binary(x) or is_atom(x) or is_number(x) end, list)
  end

  defp small_literal_list?(_list), do: false

  defp in_range(left, first, last, step) when is_integer(step) do
    in_range_literal(left, first, last, step)
  end

  defp in_range(left, first, last, step) do
    quoted =
      quote do
        :erlang.is_integer(unquote(left)) and :erlang.is_integer(unquote(first)) and
        :erlang.is_integer(unquote(last)) and
        ((:erlang.>(unquote(step), 0) and
          unquote(increasing_compare(left, first, last))) or
         (:erlang.<(unquote(step), 0) and
          unquote(decreasing_compare(left, first, last))))
      end

    in_range_step(quoted, left, first, step)
  end

  defp in_range_literal(left, first, first, _step) when is_integer(first) do
    quote do: :erlang."=:="(unquote(left), unquote(first))
  end

  defp in_range_literal(left, first, last, step) when step > 0 do
    quoted =
      quote do
        :erlang.andalso(
          :erlang.is_integer(unquote(left)),
          unquote(increasing_compare(left, first, last))
        )
      end

    in_range_step(quoted, left, first, step)
  end

  defp in_range_literal(left, first, last, step) when step < 0 do
    quoted =
      quote do
        :erlang.andalso(
          :erlang.is_integer(unquote(left)),
          unquote(decreasing_compare(left, first, last))
        )
      end

    in_range_step(quoted, left, first, step)
  end

  defp in_range_step(quoted, _left, _first, step) when step == 1 or step == -1 do
    quoted
  end

  defp in_range_step(quoted, left, first, step) do
    quote do
      :erlang.andalso(
        unquote(quoted),
        :erlang."=:="(:erlang.rem(unquote(left) - unquote(first), unquote(step)), 0)
      )
    end
  end

  defp in_list(left, head, tail, expand, right, in_body?) do
    [head | tail] = :lists.map(&comp(left, &1, expand, right, in_body?), [head | tail])
    :lists.foldl(&quote(do: :erlang.orelse(unquote(&2), unquote(&1))), head, tail)
  end

  defp comp(left, {:|, _, [head, tail]}, expand, right, in_body?) do
    case expand.(tail) do
      [] ->
        quote(do: :erlang."=:="(unquote(left), unquote(head)))

      [tail_head | tail] ->
        quote do
          :erlang.orelse(
            :erlang."=:="(unquote(left), unquote(head)),
            unquote(in_list(left, tail_head, tail, expand, right, in_body?))
          )
        end

      tail when in_body? ->
        quote do
          :erlang.orelse(
            :erlang."=:="(unquote(left), unquote(head)),
            :lists.member(unquote(left), unquote(tail))
          )
        end

      _ ->
        raise_on_invalid_args_in_2(right)
    end
  end

  defp comp(left, right, _expand, _right, _in_body?) do
    quote(do: :erlang."=:="(unquote(left), unquote(right)))
  end

  defp increasing_compare(var, first, last) do
    quote do
      :erlang.andalso(
        :erlang.>=(unquote(var), unquote(first)),
        :erlang."=<"(unquote(var), unquote(last))
      )
    end
  end

  defp decreasing_compare(var, first, last) do
    quote do
      :erlang.andalso(
        :erlang."=<"(unquote(var), unquote(first)),
        :erlang.>=(unquote(var), unquote(last))
      )
    end
  end

  defp bootstrapped?(_), do: true


  @spec inspect(Inspect.t(), keyword) :: String.t()
  def inspect(term, opts \\ []) when is_list(opts) do
    opts = Inspect.Opts.new(opts)

    limit =
      case opts.pretty do
        true -> opts.width
        false -> :infinity
      end

    doc = Inspect.Algebra.group(Inspect.Algebra.to_doc(term, opts))
    IO.iodata_to_binary(Inspect.Algebra.format(doc, limit))
  end
  
end
