defmodule EvalInWasm.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      EvalInWasm
    ]

    opts = [strategy: :one_for_one, name: EvalInWasm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
