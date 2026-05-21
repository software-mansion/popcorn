defmodule Popdoc.Wasm.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [Popdoc.Wasm]
    opts = [strategy: :one_for_one, name: Popdoc.Wasm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
