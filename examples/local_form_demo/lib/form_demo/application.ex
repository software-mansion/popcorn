defmodule FormDemo.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      FormDemoWeb.Telemetry,
      {DNSCluster, query: Application.get_env(:form_demo, :dns_cluster_query) || :ignore},
      {Phoenix.PubSub, name: FormDemo.PubSub},
      # Start a worker by calling: FormDemo.Worker.start_link(arg)
      # {FormDemo.Worker, arg},
      # Start to serve requests, typically the last entry
      FormDemoWeb.Endpoint
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: FormDemo.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    FormDemoWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
