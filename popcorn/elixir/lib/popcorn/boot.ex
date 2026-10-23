defmodule Popcorn.Boot do
  @moduledoc false

  # Prepares the VM once it boots, then starts the entrypoint application (if there's one)
  # and tells Popcorn's JS whether the VM is ready.

  # Preloaded in Popcorn's VM
  @compile {:no_warn_undefined, :wasm}

  @spec start() :: :ok
  def start do
    prepare()
    send_marker("boot_ready")
  end

  @spec start([charlist()]) :: :ok
  def start([app]) do
    prepare()

    case :application.ensure_all_started(List.to_atom(app)) do
      {:ok, _apps} ->
        send_marker("boot_ready")

      _error ->
        send_marker("boot_failed")
        :erlang.halt(1)
    end
  end

  # Native host name lookups run inet_gethost, unsupported in Popcorn,
  # so names are looked up in files instead.
  # An inetrc file can't set it, as reading one needs the Erlang parser.
  defp prepare do
    case :inet_db.res_option(:lookup) -- [:native] do
      [] -> :inet_db.set_lookup([:file])
      lookup -> :inet_db.set_lookup(lookup)
    end
  end

  defp send_marker(type), do: :wasm.send(%{"_popcorn" => %{"t" => type}})
end
