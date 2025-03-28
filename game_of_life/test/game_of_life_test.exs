defmodule GameOfLifeTest do
  use ExUnit.Case
  doctest GameOfLife

  test "greets the world" do
    assert GameOfLife.hello() == :world
  end
end
