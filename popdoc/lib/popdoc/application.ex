defmodule Popdoc.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [Popdoc.Evaluator]
    opts = [strategy: :one_for_one, name: Popdoc.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
