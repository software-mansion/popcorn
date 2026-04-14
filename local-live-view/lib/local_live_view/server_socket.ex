defmodule LocalLiveView.ServerSocket do
  @moduledoc false

  @doc """
  Syncs the given attrs to the server-side mirror.
  Must be called from within a LocalLiveView callback (handle_event, handle_info).
  """
  def mirror_sync(payload) do
    send(self(), {:llv_mirror_sync, payload})
  end
end
