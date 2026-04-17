defmodule LocalThermostat.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Registry, keys: :unique, name: LocalLiveView.ChannelRegistry},
      LocalThermostatWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:local_thermostat, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: LocalThermostat.PubSub},
      # Start a worker by calling: LocalThermostat.Worker.start_link(arg)
      # {LocalThermostat.Worker, arg},
      # Start to serve requests, typically the last entry
      LocalThermostatWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: LocalThermostat.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    LocalThermostatWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
