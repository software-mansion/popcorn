# Stub: AtomVM's ets has no table enumeration (no :ets.select or
# :ets.match_delete — see popcorn's patches/otp/stdlib/ets.erl for what
# exists), which Phoenix.Config uses to diff stale config keys and clear
# cached entries. Neither matters in the WASM runtime: config never changes
# after boot (no code reloading, so config_change/3 never fires), the table
# is always fresh at init, and caches therefore never go stale. This is a
# faithful copy of Phoenix.Config (phoenix 1.8) with `update/3` reduced to a
# plain insert and `clear_cache/1` a no-op.

module = Phoenix.Config

:code.which(module)
|> case do
  :non_existing -> :ok
  path -> File.rm(path)
end

:code.delete(module)
:code.purge(module)

defmodule module do
  @moduledoc false

  use GenServer

  def start_link({module, config, defaults, opts}) do
    permanent = Keyword.keys(defaults)
    GenServer.start_link(__MODULE__, {module, config, permanent}, opts)
  end

  def put(module, key, value) do
    :ets.insert(module, {key, value})
  end

  def permanent(module, key, value) do
    pid = :ets.lookup_element(module, :__config__, 2)
    GenServer.call(pid, {:permanent, key, value})
  end

  def cache(module, key, fun) do
    case :ets.lookup(module, key) do
      [{^key, :cache, val}] ->
        val

      [] ->
        case fun.(module) do
          {:cache, val} ->
            :ets.insert(module, {key, :cache, val})
            val

          {:nocache, val} ->
            val
        end
    end
  end

  def clear_cache(_module) do
    # :ets.match_delete is unavailable; nothing ever invalidates the cache in
    # the WASM runtime (config is immutable after boot), so this is safe.
    :ok
  end

  def from_env(otp_app, module, defaults) do
    config = fetch_config(otp_app, module)

    merge(defaults, config)
  end

  defp fetch_config(otp_app, module) do
    case Application.fetch_env(otp_app, module) do
      {:ok, conf} -> conf
      :error -> []
    end
  end

  def merge(a, b), do: Keyword.merge(a, b, &merger/3)

  defp merger(_k, v1, v2) do
    if Keyword.keyword?(v1) and Keyword.keyword?(v2) do
      Keyword.merge(v1, v2, &merger/3)
    else
      v2
    end
  end

  def config_change(module, changed, removed) do
    pid = :ets.lookup_element(module, :__config__, 2)
    GenServer.call(pid, {:config_change, changed, removed})
  end

  # Callbacks

  def init({module, config, permanent}) do
    :ets.new(module, [:named_table, :public, read_concurrency: true])
    update(module, config, [])
    :ets.insert(module, {:__config__, self()})
    {:ok, {module, [:__config__ | permanent]}}
  end

  def handle_call({:permanent, key, value}, _from, {module, permanent}) do
    :ets.insert(module, {key, value})
    {:reply, :ok, {module, [key | permanent]}}
  end

  def handle_call({:config_change, changed, removed}, _from, {module, permanent}) do
    cond do
      changed = changed[module] ->
        update(module, changed, permanent)
        {:reply, :ok, {module, permanent}}

      module in removed ->
        {:stop, :normal, :ok, {module, permanent}}

      true ->
        clear_cache(module)
        {:reply, :ok, {module, permanent}}
    end
  end

  # The stale-key diff of the original needs :ets.select; a fresh-at-init
  # table with immutable config makes the diff a no-op anyway.
  defp update(module, config, _permanent) do
    :ets.insert(module, config)
    clear_cache(module)
  end
end
