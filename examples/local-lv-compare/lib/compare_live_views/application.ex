defmodule CompareLiveViews.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      CompareLiveViewsWeb.Telemetry,
      {DNSCluster,
       query: Application.get_env(:compare_live_views, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: CompareLiveViews.PubSub},
      # Start a worker by calling: CompareLiveViews.Worker.start_link(arg)
      # {CompareLiveViews.Worker, arg},
      # Start to serve requests, typically the last entry
      CompareLiveViewsWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: CompareLiveViews.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    CompareLiveViewsWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
