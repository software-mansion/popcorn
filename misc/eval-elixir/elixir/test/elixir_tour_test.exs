defmodule EvalElixirTest do
  use ExUnit.Case
  doctest EvalElixir

  test "greets the world" do
    assert EvalElixir.hello() == :world
  end
end
