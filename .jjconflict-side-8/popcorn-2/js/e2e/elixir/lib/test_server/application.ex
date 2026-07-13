defmodule TestServer.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      TestServer
    ]

    opts = [strategy: :one_for_one, name: TestServer.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
