defmodule LocalLiveView.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    ensure_plug_crypto_key_cache()

    children = [
      {DynamicSupervisor, name: LocalLiveView.ChannelSupervisor, strategy: :one_for_one},
      LocalLiveView.Dispatcher
    ]

    opts = [strategy: :one_for_one, name: LocalLiveView.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Plug.Crypto caches derived keys in a named ETS table owned by the
  # :plug_crypto application. In the WASM runtime that application has no .app
  # file and never starts, but session-token signing (dispatcher join) needs
  # the table — create it by hand when the app isn't startable.
  defp ensure_plug_crypto_key_cache do
    case Application.ensure_all_started(:plug_crypto) do
      {:ok, _apps} ->
        :ok

      {:error, _reason} ->
        try do
          :ets.new(Plug.Crypto.Keys, [:named_table, :public, {:read_concurrency, true}])
        rescue
          ArgumentError -> :ok
        end

        :ok
    end
  end
end
