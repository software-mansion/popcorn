defmodule GameOfLife.Supervisor do
  use DynamicSupervisor

  def start_link(_init_arg) do
    DynamicSupervisor.start_link(__MODULE__, [], name: __MODULE__)
  end

  def start_simulation(xSize, ySize, alive_coords) do
    game_id = make_ref()

    spec =
      {GameOfLife.Simulation, game_id: game_id, size: {xSize, ySize}, alive_coords: alive_coords}

    DynamicSupervisor.start_child(__MODULE__, spec)
  end

  @impl true
  def init(_init_arg) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end
end
