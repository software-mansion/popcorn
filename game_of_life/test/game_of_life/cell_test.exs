defmodule CellTest do
  use ExUnit.Case
  alias GameOfLife.Cell

  test "start and init" do
    assert {:ok, pid} = Cell.start_link(0, 0, 3, 3, true)
    state = :sys.get_state(pid)
    assert %Cell{} = state
    assert length(state.neighbours)
    assert {0, 1} in state.neighbours
    assert {1, 0} in state.neighbours
    assert {1, 1} in state.neighbours
    assert Cell.alive?({0, 0}, 0)
  end

  test "alive? call" do
    assert {:ok, pid} = Cell.start_link(0, 0, 1, 1, true)
    assert Cell.alive?(pid, 0) == true
    assert Cell.alive?({0, 0}, 0) == true
    assert :ok = GenServer.stop(pid)

    assert {:ok, pid} = Cell.start_link(0, 0, 1, 1, false)
    assert Cell.alive?(pid, 0) == false
    assert Cell.alive?({0, 0}, 0) == false
    assert :ok = GenServer.stop(pid)
  end

  @tag skip: "Edge case, ignoring"
  test "tick with no neighbours" do
    assert {:ok, pid} = Cell.start_link(0, 0, 1, 1, true)
    assert :ok = Cell.tick(pid)
    assert_receive {:status_response, {0, 0}, false}
  end

  test "tick with one neighbour" do
    assert {:ok, pid_0} = Cell.start_link(0, 0, 2, 1, true)
    assert {:ok, pid_1} = Cell.start_link(1, 0, 2, 1, true)
    assert :ok = Cell.tick(pid_0)
    assert :ok = Cell.tick(pid_1)
    assert_receive {:"$gen_cast", {:status, {0, 0}, false}}
    assert_receive {:"$gen_cast", {:status, {1, 0}, false}}
  end
end
