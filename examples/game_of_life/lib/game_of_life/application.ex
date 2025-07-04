defmodule GameOfLife.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: GameOfLife.Simulation.registry()},
      GameOfLife.Supervisor,
      {GameOfLife.Ui, %{size: 10}},
      GameOfLife
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: GameOfLife.AppSupervisor]
    Supervisor.start_link(children, opts)
  end
end
