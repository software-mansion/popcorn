defmodule HelloPopcorn.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      HelloPopcorn
    ]

    opts = [strategy: :one_for_one, name: HelloPopcorn.Supervisor]
    Supervisor.start_link(children, opts)
  end
end 
