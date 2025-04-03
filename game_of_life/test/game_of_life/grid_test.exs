defmodule GameOfLife.GridTest do
  use ExUnit.Case

  @registry GameOfLife.CellRegistry

  test "cells servers die with grid" do
    assert {:ok, grid} = GameOfLife.Grid.start_link(3, 3, [])
    assert [_one] = Registry.lookup(@registry, {0, 0})
    assert :ok = GenServer.stop(grid)
    assert [] = Registry.lookup(@registry, {0, 0})
  end

  test "live cell with zero live neighbours die" do
    assert {:ok, grid} = GameOfLife.Grid.start_link(3, 3, [{1, 1}])
    assert {:ok, grid} = GameOfLife.Grid.start_link(3, 3, [{1, 1}])
  end
end
