defmodule IexWasm.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      IexWasm
    ]

    opts = [strategy: :one_for_one, name: IexWasm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
