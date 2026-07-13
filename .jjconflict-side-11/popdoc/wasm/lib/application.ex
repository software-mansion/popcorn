defmodule PopdocWasm.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [PopdocWasm]
    opts = [strategy: :one_for_one, name: PopdocWasm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
