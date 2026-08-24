defmodule LocalLiveView.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    ensure_plug_crypto_key_cache()

    children = [
      {LocalLiveView.Endpoint, endpoint_config()},
      LocalLiveView.Dispatcher
    ]

    opts = [strategy: :one_for_one, name: LocalLiveView.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # The signing secret is a constant on purpose: session tokens are signed
  # when the dispatcher renders a view's container (live_render) and verified
  # by the channel inside the same browser-local runtime — they never cross a
  # trust boundary, the token machinery is only exercised because the stock
  # channel requires it.
  #
  # hibernate_after is :infinity because :proc_lib.hibernate is untested on
  # AtomVM; an idle local view must not gamble on it.
  defp endpoint_config do
    [
      server: false,
      secret_key_base: "local-live-view-browser-only-secret-key-base-000000000000000000",
      live_view: [signing_salt: "local-live-view", hibernate_after: :infinity]
    ]
  end

  # FIXME: verify when the application startup fails, remove the manual table creation
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
