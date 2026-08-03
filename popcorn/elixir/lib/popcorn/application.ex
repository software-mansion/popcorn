defmodule Popcorn.Application do
  @moduledoc false

  use Application
  require Logger

  @impl true
  def start(_type, _args) do
    if Popcorn.Wasm.available?() and Code.ensure_loaded?(Req) do
      install_req_adapter()
    end

    Supervisor.start_link([], strategy: :one_for_one, name: Popcorn.Supervisor)
  end

  defp install_req_adapter do
    opts = Application.get_env(:req, :default_options, [])

    if not Keyword.has_key?(opts, :adapter) do
      opts = Keyword.put(opts, :adapter, Popcorn.Fetch)
      Application.put_env(:req, :default_options, opts)
      Logger.debug(app: :popcorn, message: "using Popcorn.Fetch as Req's adapter")
    end
  end
end
