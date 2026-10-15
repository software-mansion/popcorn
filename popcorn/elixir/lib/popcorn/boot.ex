defmodule Popcorn.Boot do
  @moduledoc false

  # Starts the entrypoint application once the VM boots, and tells Popcorn's
  # JS whether it started. The VM calls it with
  # `-run Elixir.Popcorn.Boot start <app>`, which unlike `-eval` needs no
  # Erlang parser or evaluator, so they can be left out.

  # Preloaded in Popcorn's VM
  @compile {:no_warn_undefined, :wasm}

  @spec start([charlist()]) :: :ok
  def start([app]) do
    case :application.ensure_all_started(List.to_atom(app)) do
      {:ok, _apps} ->
        send_marker("boot_ready")

      _error ->
        send_marker("boot_failed")
        :erlang.halt(1)
    end
  end

  defp send_marker(type), do: :wasm.send(%{"_popcorn" => %{"t" => type}})
end
