defmodule LlvIntegration.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {Phoenix.PubSub, name: LlvIntegration.PubSub},
      LlvIntegrationWeb.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one, name: LlvIntegration.Supervisor)
  end
end
