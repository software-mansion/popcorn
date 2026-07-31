defmodule Burrito.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      BurritoWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:burrito, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: Burrito.PubSub},
      BurritoWeb.Endpoint
    ]

    opts = [strategy: :one_for_one, name: Burrito.Supervisor]
    Supervisor.start_link(children, opts)
  end

  @impl true
  def config_change(changed, _new, removed) do
    BurritoWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
