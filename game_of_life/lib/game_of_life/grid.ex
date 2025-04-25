defmodule GameOfLife.Grid do
  use GenServer

  alias GameOfLife.Cell

  def child_spec(args) do
    game_id = Keyword.fetch!(args, :game_id)
    size = Keyword.fetch!(args, :size)

    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [game_id, size]}
    }
  end

  def start_link(game_id, size) do
    GenServer.start_link(__MODULE__, {game_id, size},
      name: GameOfLife.Simulation.grid_via_tuple(game_id)
    )
  end

  def tick(grid) do
    GenServer.call(grid, :tick)
  end

  def status(grid) do
    GenServer.call(grid, :status)
  end

  @doc false
  # Internal cast for cells to respond with status
  def notify_status(grid, coords, alive?) do
    GenServer.cast(grid, {:status, coords, alive?})
  end

  @impl true
  def init({game_id, {xSize, ySize}}) do
    grid =
      for x <- 0..(xSize - 1), y <- 0..(ySize - 1), into: %{} do
        coords = {x, y}
        {coords, GameOfLife.Simulation.cell_pid(game_id, coords)}
      end

    {:ok,
     %{
       game_id: game_id,
       grid: grid,
       xSize: xSize,
       ySize: ySize,
       epoch: 0,
       tick_progress: nil,
       respond_to: nil
     }}
  end

  @impl true
  def handle_call(:tick, from, state) do
    state = %{state | respond_to: from, tick_progress: %{}}

    state.grid |> Enum.each(fn {_coords, pid} -> Cell.tick(pid) end)
    {:noreply, state}
  end

  def handle_call(:status, _from, state) do
    grid =
      state.grid
      |> Enum.map(fn {coords, pid} -> {coords, Cell.alive?(pid)} end)
      |> to_matrix(state.xSize, state.ySize)

    {:reply, grid, state}
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
    grid_map = Map.new(flat_grid)

    for y <- 0..(ySize - 1) do
      for x <- 0..(xSize - 1) do
        grid_map[{x, y}]
      end
    end
  end
end
