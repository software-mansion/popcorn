defmodule GameOfLife.SimulationTest do
  use ExUnit.Case, async: true

  alias GameOfLife.{Grid, Simulation}

  setup do
    [game_id: make_ref()]
  end

  def grid_to_live_cells(grid) do
    grid |> Grid.to_flat_grid() |> Enum.filter(&elem(&1, 1)) |> Enum.map(&elem(&1, 0))
  end

  test "lifecycle & registry", %{game_id: id} do
    assert {:ok, _sup, %{grid_pid: grid}} =
             start_supervised({Simulation, game_id: id, size: {3, 3}, alive_coords: []})

    assert grid != nil
    assert Simulation.grid_pid(id) == grid
    assert GenServer.whereis(Simulation.grid_via_tuple(id)) == grid

    cell = Simulation.cell_pid(id, {0, 0})
    assert cell != nil
    assert GenServer.whereis(Simulation.cell_via_tuple(id, {0, 0})) == cell

    assert Simulation.cell_pid(id, {3, 3}) == nil

    assert :ok = stop_supervised(Simulation)
    refute Process.alive?(grid)
    refute Process.alive?(cell)
  end

  test "Horizontal/vertical swap pattern", %{game_id: id} do
    size = {3, 3}
    # 0 X 0
    # 0 X 0
    # 0 X 0
    vertical_alive = [{1, 0}, {1, 1}, {1, 2}]

    # 0 0 0
    # X X X
    # 0 0 0
    horizontal_alive = [{0, 1}, {1, 1}, {2, 1}]

    assert {:ok, _sup, %{grid_pid: pid}} =
             start_supervised({Simulation, game_id: id, size: size, alive_coords: vertical_alive})

    assert vertical_alive ==
             Grid.status(pid) |> grid_to_live_cells()

    for _i <- 1..5 do
      assert grid = Grid.tick(pid)
      assert horizontal_alive == grid_to_live_cells(grid)
      assert grid = Grid.tick(pid)
      assert vertical_alive == grid_to_live_cells(grid)
    end
  end
end
