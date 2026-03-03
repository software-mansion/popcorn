defmodule GameOfLife.Supervisor do
  @moduledoc """
  A DynamicSupervisor used to start and supervise simulations (`GameOfLife.Simulation`)
  """
  use DynamicSupervisor

  @doc """
  Starts #{__MODULE__}. Initial argument is ignored
  """
  @spec start_link(ignored :: term()) :: Supervisor.on_start()
  def start_link(_init_arg) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  @doc """
  Starts a `GameOfLife.Simulation` under the supervisor.
  Takes the sizes along x and y axis, and a list of coordinates for the intially alive cells.
  Returns the value returned from `GameOfLife.Simulation.start_link/1`
  """
  @spec start_simulation(
          xSize :: pos_integer(),
          ySize :: pos_integer(),
          alive_coords :: [GameOfLife.Grid.coords()]
        ) :: {:ok, pid(), info :: %{grid_pid: pid()}} | {:error, term()}
  def start_simulation(xSize, ySize, alive_coords) do
    game_id = make_ref()

    spec =
      {GameOfLife.Simulation, game_id: game_id, size: {xSize, ySize}, alive_coords: alive_coords}

    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  def stop_simulation(pid) do
    DynamicSupervisor.terminate_child(__MODULE__, pid)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
