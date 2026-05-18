defmodule LocalLiveView.Application.Host do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: LocalLiveView.ChannelRegistry}
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: LocalLiveView.Supervisor)
  end
end
