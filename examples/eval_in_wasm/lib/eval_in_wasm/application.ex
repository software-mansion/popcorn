defmodule EvalInWasm.Application do
  @moduledoc false

  use Application

  @should_start Mix.env() != :test

  @impl true
  def start(_type, _args) do
    children = if @should_start, do: [EvalInWasm], else: []

    opts = [strategy: :one_for_one, name: EvalInWasm.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
