defmodule GameOfLife.Cell do
  use GenServer

  alias GameOfLife.{Grid, Simulation}

  @type t :: %__MODULE__{
          game_id: Simulation.id(),
          coords: Grid.coords(),
          neighbours: [Grid.coords()],
          epochs: [{non_neg_integer(), boolean()}],
          pending_epoch: %{atom() => term()}
        }
  defstruct game_id: nil,
            coords: nil,
            neighbours: [],
            epochs: [],
            pending_epoch: nil

  @doc """
  Child specification to start `GameOfLife.Cell` as part of supervision tree
  """
  def child_spec(args) do
    game_id = Keyword.fetch!(args, :game_id)
    size = Keyword.fetch!(args, :size)
    coords = Keyword.fetch!(args, :coords)
    alive? = Keyword.fetch!(args, :alive?)

    %{
      id: coords,
      start: {__MODULE__, :start_link, [game_id, coords, size, alive?]}
    }
  end

  @doc """
  Starts a cell with provided initial state and register it in `GameOfLife.Simulation.registry/0`
  """
  @spec start_link(Simulation.id(), Grid.coords(), Grid.size(), boolean()) :: GenServer.on_start()
  def start_link(game_id, coords, size, alive?) do
    GenServer.start_link(__MODULE__, {game_id, coords, size, alive?},
      name: GameOfLife.Simulation.cell_via_tuple(game_id, coords)
    )
  end

  @doc """
  Gets the status of `GameOfLife.Cell` in provided epoch.

  Returns boolean status
  """
  @spec alive?(GenServer.server(), epoch) :: boolean() when epoch: :current | non_neg_integer()
  def alive?(cell, epoch \\ :current) do
    GenServer.call(cell, {:alive?, epoch})
  end

  @doc """
  Starts a new epoch for the cell.

  It initiates gathering status of the neighbours that results in calculating a new status
  """
  def tick(cell) do
    GenServer.cast(cell, {:tick, self()})
  end

  @impl true
  def init({game_id, {x, y}, {xSize, ySize}, alive?}) do
    {:ok,
     %__MODULE__{
       game_id: game_id,
       coords: {x, y},
       neighbours: neighbours(x, y, xSize, ySize),
       epochs: [{0, alive?}]
     }}
  end

  @impl true
  def handle_call({:alive?, :current}, _from, state) do
    [{_current_epoch, alive?} | _] = state.epochs
    {:reply, alive?, state}
  end

  def handle_call({:alive?, epoch}, _from, state) do
    {_epoch, alive?} = List.keyfind!(state.epochs, epoch, 0)
    {:reply, alive?, state}
  end

  @impl true
  def handle_cast({:tick, notify}, state) do
    [{current_epoch, alive?} | _] = state.epochs

    for neighbour <- state.neighbours do
      cell = GameOfLife.Simulation.cell_via_tuple(state.game_id, neighbour)
      GenServer.cast(cell, {:get_status, current_epoch, self()})
    end

    epoch_stats = %{
      previously_alive?: alive?,
      notify: notify,
      pending: length(state.neighbours),
      alive_neighbours: 0
    }

    {:noreply, %{state | pending_epoch: epoch_stats}}
  end

  def handle_cast({:get_status, epoch, reply_to}, state) do
    {_epoch, alive?} = List.keyfind!(state.epochs, epoch, 0)
    GenServer.cast(reply_to, {:status_response, epoch, alive?})
    {:noreply, state}
  end

  def handle_cast({:status_response, epoch, alive?}, %{pending_epoch: stats} = state) do
    alive_diff = if alive?, do: 1, else: 0

    new_pending = stats.pending - 1
    alive_neighbours = stats.alive_neighbours + alive_diff
    stats = %{stats | pending: new_pending, alive_neighbours: alive_neighbours}

    epochs =
      if new_pending == 0 do
        new_alive? =
          if stats.previously_alive? do
            alive_neighbours in [2, 3]
          else
            alive_neighbours == 3
          end

        Grid.notify_status(stats.notify, state.coords, new_alive?)

        [{epoch, new_alive?} | state.epochs]
        |> Enum.take(3)
      else
        state.epochs
      end

    {:noreply, %{state | pending_epoch: stats, epochs: epochs}}
  end

  @doc """
  Calculates neighbouring coordinates for provided `x` and `y` on the grid of provided size
  """
  @spec neighbours(
          x :: non_neg_integer(),
          y :: non_neg_integer(),
          xSize :: pos_integer(),
          ySize :: pos_integer()
        ) :: [Grid.coords()]
  def neighbours(x, y, xSize, ySize) do
    for dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx != 0 or dy != 0 do
      {x + dx, y + dy}
    end
    |> Enum.filter(&inside_grid?(&1, xSize, ySize))
  end

  defp inside_grid?({x, y}, xSize, ySize) do
    x >= 0 and x < xSize and y >= 0 and y < ySize
  end
end
