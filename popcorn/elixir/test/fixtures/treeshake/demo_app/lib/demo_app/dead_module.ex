defmodule DemoApp.DeadModule do
  @moduledoc false
  # Never referenced anywhere — dead module.
  def dead_a(x), do: x + 1
  def dead_b(x), do: dead_a(x) * 2
end
