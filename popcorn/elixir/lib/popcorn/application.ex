defmodule Popcorn.Application do
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    install_req_adapter()

    Supervisor.start_link([Popcorn.Proxy], strategy: :one_for_one, name: Popcorn.Supervisor)
  end

  if Code.ensure_loaded?(Req) do
    require Logger

    defp install_req_adapter do
      if Popcorn.Wasm.available?() and Code.ensure_loaded?(Req) do
        opts = Application.get_env(:req, :default_options, [])

        if not Keyword.has_key?(opts, :adapter) do
          opts = Keyword.put(opts, :adapter, Popcorn.Fetch.adapter())
          Application.put_env(:req, :default_options, opts)
          Logger.debug(app: :popcorn, message: "using Popcorn.Fetch as Req's adapter")
        end
      end
    end
  else
    defp install_req_adapter, do: :ok
  end
end
