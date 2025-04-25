defmodule GameOfLife.Cell do
  use GenServer

  alias GameOfLife.Grid

  # @type t :: %__MODULE__{
  #         alive?: boolean(),
  #         neighbours: [pid()],
  #         epoch: non_neg_integer()
  #       }
  defstruct game_id: nil,
            coords: nil,
            neighbours: [],
            parent: nil,
            epochs: [],
            pending_epoch: nil

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

  def start_link(game_id, coords, size, alive?) do
    GenServer.start_link(__MODULE__, {game_id, coords, size, alive?},
      name: GameOfLife.Simulation.cell_via_tuple(game_id, coords)
    )
  end

  def alive?(cell, epoch \\ :current) do
    GenServer.call(cell, {:alive?, epoch})
  end

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

  def neighbours(x, y, xSize, ySize) do
    for dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx != 0 or dy != 0 do
      {x + dx, y + dy}
    end
    |> Enum.filter(fn {x, y} -> x >= 0 and x < xSize and y >= 0 and y < ySize end)
  end
end
