defmodule EvalElixir.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      EvalElixir,
      EvalElixir.Evaluator
    ]

    # max_restarts can't be infinity, so we pass a 'big enough' number
    max_restarts = 2 ** 32
    opts = [strategy: :one_for_one, max_restarts: max_restarts, name: EvalElixir.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
