defmodule GameOfLife.Grid do
  use GenServer

  alias GameOfLife.Cell

  def start_link(xSize, ySize, alive_list) do
    GenServer.start_link(__MODULE__, {xSize, ySize, alive_list})
  end

  def notify_status(grid, coords, alive?) do
    GenServer.cast(grid, {:status, coords, alive?})
  end

  def tick(grid) do
    GenServer.call(grid, :tick)
  end

  @impl true
  def init({xSize, ySize, alive_list}) do
    grid =
      for x <- 0..(xSize - 1), y <- 0..(ySize - 1), into: %{} do
        alive? = {x, y} in alive_list
        # FIXME: Supervise cells
        {:ok, pid} = Cell.start_link(x, y, xSize, ySize, alive?)
        {{x, y}, pid}
      end

    {:ok,
     %{grid: grid, xSize: xSize, ySize: ySize, epoch: 0, tick_progress: nil, respond_to: nil}}
  end

  @impl true
  def handle_call(:tick, from, state) do
    state = %{state | respond_to: from, tick_progress: %{}}

    state.grid |> Enum.each(fn {_coords, pid} -> Cell.tick(pid) end)
    {:noreply, state}
  end

  @impl true
  def handle_cast({:status, coords, alive?}, state) do
    state = put_in(state.tick_progress[coords], alive?)

    state =
      if map_size(state.tick_progress) == map_size(state.grid) do
        status = to_matrix(state.tick_progress, state.xSize, state.ySize)
        GenServer.reply(state.respond_to, status)
        %{state | respond_to: nil, tick_progress: nil}
      else
        state
      end

    {:noreply, state}
  end

  def to_flat_grid(matrix) do
    for {row, y} <- Stream.with_index(matrix), {entry, x} <- Stream.with_index(row) do
      {{x, y}, entry}
    end
  end

  def to_matrix(flat_grid, xSize, ySize) do
    for y <- 0..(ySize - 1) do
      for x <- 0..(xSize - 1) do
        flat_grid[{x, y}]
      end
    end
  end
end
