defmodule GameOfLife.Simulation do
  @moduledoc """
  A main process (a supervisor) for one simulation.
  Starts a `GameOfLife.Grid` and `GameOfLife.Cell`s and registers them
  via `registry/1` Registry.

  Each `GameOfLife.Grid` is registered under `game_id` key,
  and each `GameOfLife.Cell` under `{game_id, {x, y}}` key
  """
  use Supervisor

  alias GameOfLife.{Cell, Grid}

  @registry_name GameOfLife.Registry

  @typedoc """
  Term identifying a simulation
  """
  @type id() :: term()

  @doc """
  Returns the name of the Registry that is used by all simulations.
  """
  @spec registry() :: atom()
  def registry(), do: @registry_name

  @typedoc """
  Options used by `start_link/1`
  """
  @type option() ::
          {:game_id, id()}
          | {:size, Grid.size()}
          | {:alive_coords, [Grid.coords()]}

  @doc """
  Starts a simulation.

  Expects a keyword with following arguments:
  - `:game_id` - unique term used as id
  - `:size` - a `{xSize, ySize}` tuple with grid size
  - `:alive_coords` - a list of `t:GameOfLife.Grid.coords/0` with cells alive at the beginning

  Returns the simulation pid and info map containing a pid of `GameOfLife.Grid` (under `:grid_pid` key)
  """
  @spec start_link(args :: [option()]) ::
          {:ok, pid(), info :: %{grid_pid: pid()}} | {:error, term()}
  def start_link(args) do
    game_id = Keyword.fetch!(args, :game_id)

    with {:ok, sup_pid} <- Supervisor.start_link(__MODULE__, args) do
      {:ok, sup_pid, %{grid_pid: grid_pid(game_id)}}
    end
  end

  @doc """
  Returns a tuple that can be used as `t:GenServer.name/0` to call a `GameOfLife.Cell`
  """
  @spec cell_via_tuple(term(), Grid.coords()) :: GenServer.name()
  def cell_via_tuple(game_id, coords) do
    {:via, Registry, {@registry_name, {game_id, coords}}}
  end

  @doc """
  Obtains a pid of `GameOfLife.Cell` with provided coords
  """
  @spec cell_pid(term(), Grid.coords()) :: pid() | nil
  def cell_pid(game_id, coords) do
    with [{pid, _meta}] <- Registry.lookup(@registry_name, {game_id, coords}) do
      pid
    else
      _other -> nil
    end
  end

  @doc """
  Returns a tuple that can be used as `t:GenServer.name/0` to call a `GameOfLife.Grid`
  """
  @spec grid_via_tuple(term()) :: GenServer.name()
  def grid_via_tuple(game_id) do
    {:via, Registry, {@registry_name, game_id}}
  end

  @doc """
  Obtains a pid of `GameOfLife.Grid` for provided `game_id`
  """
  @spec grid_pid(term()) :: pid() | nil
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
