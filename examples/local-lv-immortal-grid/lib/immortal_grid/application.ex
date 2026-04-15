defmodule ImmortalGrid.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      ImmortalGridWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:immortal_grid, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: ImmortalGrid.PubSub},
      ImmortalGrid.GridState,
      ImmortalGridWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: ImmortalGrid.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    ImmortalGridWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
