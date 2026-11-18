defmodule Popcorn.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    {:module, DateTime} = Code.ensure_loaded(DateTime)
    {:module, Calendar.ISO} = Code.ensure_loaded(Calendar.ISO)
    {:module, MapSet} = Code.ensure_loaded(MapSet)
    :ok = Popcorn.Fetch.install_req_adapter()

    Supervisor.start_link([Popcorn.Proxy], strategy: :one_for_one, name: Popcorn.Supervisor)
  end
end
