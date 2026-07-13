defmodule LocalLvCheckout.Application do
  # See https://elixir.hexdocs.pm/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      LocalLvCheckoutWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:local_lv_checkout, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: LocalLvCheckout.PubSub},
      # Start a worker by calling: LocalLvCheckout.Worker.start_link(arg)
      # {LocalLvCheckout.Worker, arg},
      # Start to serve requests, typically the last entry
      LocalLvCheckoutWeb.Endpoint
    ]

    # See https://elixir.hexdocs.pm/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: LocalLvCheckout.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    LocalLvCheckoutWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
