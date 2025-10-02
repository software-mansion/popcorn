defmodule LocalLiveView.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      LocalLiveView.Dispatcher,
      {DynamicSupervisor, name: LocalLiveView.Server.Supervisor, strategy: :one_for_one}
    ]

    opts = [strategy: :one_for_one, name: LocalLiveView.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
