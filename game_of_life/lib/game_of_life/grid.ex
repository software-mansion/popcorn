defmodule GameOfLife.Grid do
  use GenServer

  alias GameOfLife.{Cell, Simulation}

  @typedoc """
  Zero-based coordinates for a `GameOfLife.Cell`
  """
  @type coords :: {non_neg_integer(), non_neg_integer()}

  @typedoc """
  Tuple describing size of a `GameOfLife.Grid`
  """
  @type size :: {pos_integer(), pos_integer()}

  def child_spec(args) do
    game_id = Keyword.fetch!(args, :game_id)
    size = Keyword.fetch!(args, :size)

    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [game_id, size]}
    }
  end

  @spec start_link(Simulation.id(), size()) :: GenServer.on_start()
  def start_link(game_id, size) do
    GenServer.start_link(__MODULE__, {game_id, size},
      name: GameOfLife.Simulation.grid_via_tuple(game_id)
    )
  end

  @doc """
  Starts a next generation in simulation

  Returns list of lists with booleans representing alive status
  """
  @spec tick(GenServer.server()) :: [[boolean()]]
  def tick(grid) do
    GenServer.call(grid, :tick)
  end

  @doc """
  Gets a status of all grid cells without starting a next generation
  """
  @spec status(GenServer.server()) :: [[boolean()]]
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

  @doc """
  Converts a matrix in a form of list of lists into enumerable of tuples with coordinates
  """
  @spec to_flat_grid([[item]]) :: [{coords(), item}] when item: term()
  def to_flat_grid(matrix) do
    for {row, y} <- Stream.with_index(matrix), {entry, x} <- Stream.with_index(row) do
      {{x, y}, entry}
    end
  end

  @doc """
  Converts enumerable of `{coordinates, value}` tuples into a matrix (a list of lists)
  """
  @spec to_matrix(Enumerable.t({coords(), item}), xSize :: pos_integer(), ySize :: pos_integer()) ::
          [[item]]
        when item: term()
  def to_matrix(flat_grid, xSize, ySize) do
    grid_map = Map.new(flat_grid)

    for y <- 0..(ySize - 1) do
      for x <- 0..(xSize - 1) do
        grid_map[{x, y}]
      end
    end
  end
end
