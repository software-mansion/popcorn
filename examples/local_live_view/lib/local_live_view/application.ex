defmodule LocalLiveView.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      LocalLiveView
    ]

    opts = [strategy: :one_for_one, name: LocalLiveView.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
