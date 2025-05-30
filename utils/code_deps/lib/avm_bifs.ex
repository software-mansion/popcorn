defmodule AVMBIFs do
  def list() do
    MapSet.new([
      {:erlang, :self, 0},
      {:erlang, :byte_size, 1},
      {:erlang, :bit_size, 1},
      {:erlang, :is_atom, 1},
      {:erlang, :is_binary, 1},
      {:erlang, :is_boolean, 1},
      {:erlang, :is_float, 1},
      {:erlang, :is_function, 1},
      {:erlang, :is_integer, 1},
      {:erlang, :is_list, 1},
      {:erlang, :is_number, 1},
      {:erlang, :is_pid, 1},
      {:erlang, :is_reference, 1},
      {:erlang, :is_tuple, 1},
      {:erlang, :is_map, 1},
      {:erlang, :is_map_key, 2},
      {:erlang, :length, 1},
      {:erlang, :hd, 1},
      {:erlang, :tl, 1},
      {:erlang, :element, 2},
      {:erlang, :tuple_size, 1},
      {:erlang, :map_size, 1},
      {:erlang, :map_get, 2},
      {:erlang, :add, 2},
      {:erlang, :sub, 2},
      {:erlang, :mul, 2},
      {:erlang, :div, 2},
      {:erlang, :neg, 1},
      {:erlang, :abs, 1},
      {:erlang, :rem, 2},
      {:erlang, :ceil, 1},
      {:erlang, :floor, 1},
      {:erlang, :round, 1},
      {:erlang, :trunc, 1},
      {:erlang, :bor, 2},
      {:erlang, :band, 2},
      {:erlang, :bxor, 2},
      {:erlang, :bsl, 2},
      {:erlang, :bsr, 2},
      {:erlang, :bnot, 1},
      {:erlang, :not, 1},
      {:erlang, :and, 2},
      {:erlang, :or, 2},
      {:erlang, :xor, 2},
      {:erlang, :equal_to, 2},
      {:erlang, :not_equal_to, 2},
      {:erlang, :exactly_equal_to, 2},
      {:erlang, :exactly_not_equal_to, 2},
      {:erlang, :greater_than, 2},
      {:erlang, :less_than, 2},
      {:erlang, :less_than_or_equal, 2},
      {:erlang, :greater_than_or_equal, 2},
      {:erlang, :get, 1},
      {:erlang, :min, 2},
      {:erlang, :max, 2},
      {:erlang, :size, 1},
      {:erlang, :byte_size, 1},
      {:erlang, :tuple_size, 1}
    ])
  end
end
