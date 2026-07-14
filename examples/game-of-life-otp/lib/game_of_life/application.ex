defmodule GameOfLife.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children =
      [
        {Registry, keys: :unique, name: GameOfLife.Simulation.registry()},
        GameOfLife.Supervisor
      ] ++ ui()

    opts = [strategy: :one_for_one, name: GameOfLife.AppSupervisor]
    Supervisor.start_link(children, opts)
  end

  # The bridge exists only inside the OTP/WASM runtime; on the host BEAM
  # (e.g. `mix test`) the app runs without the UI.
  defp ui() do
    if Popcorn.Wasm.available?() do
      [{GameOfLife.Ui, %{size: 20}}]
    else
      []
    end
  end
end
