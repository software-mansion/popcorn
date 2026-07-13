defmodule HelloPopcornOtp.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      HelloPopcornOtp
    ]

    opts = [strategy: :one_for_one, name: HelloPopcornOtp.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
