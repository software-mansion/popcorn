defmodule GameOfLife.Simulation do
  use Supervisor

  alias GameOfLife.{Cell, Grid}

  @registry_name GameOfLife.Registry

  def registry(), do: @registry_name

  def start_link(args) do
    game_id = Keyword.fetch!(args, :game_id)

    with {:ok, sup_pid} <- Supervisor.start_link(__MODULE__, args) do
      {:ok, sup_pid, %{grid_pid: grid_pid(game_id)}}
    end
  end

  def cell_via_tuple(game_id, coords) do
    {:via, Registry, {@registry_name, {game_id, coords}}}
  end

  def cell_pid(game_id, coords) do
    with [{pid, _meta}] <- Registry.lookup(@registry_name, {game_id, coords}) do
      pid
    else
      _other -> nil
    end
  end

  def grid_via_tuple(game_id) do
    {:via, Registry, {@registry_name, game_id}}
  end

  def grid_pid(game_id) do
    with [{pid, _meta}] <- Registry.lookup(@registry_name, game_id) do
      pid
    else
      _other -> nil
    end
  end

  @impl true
  def init(args) do
    game_id = Keyword.fetch!(args, :game_id)
    {xSize, ySize} = Keyword.fetch!(args, :size)
    alive_set = args |> Keyword.fetch!(:alive_coords) |> MapSet.new()

    children = [
      {Grid, game_id: game_id, size: {xSize, ySize}}
    ]

    cells =
      for x <- 0..(xSize - 1), y <- 0..(ySize - 1) do
        coords = {x, y}
        alive? = coords in alive_set
        {Cell, game_id: game_id, coords: coords, size: {xSize, ySize}, alive?: alive?}
      end

    Supervisor.init(cells ++ children, strategy: :one_for_all)
  end
end
