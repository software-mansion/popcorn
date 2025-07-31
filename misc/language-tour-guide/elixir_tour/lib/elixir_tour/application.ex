defmodule ElixirTour.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      ElixirTour
    ]

    opts = [strategy: :one_for_one, name: ElixirTour.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
