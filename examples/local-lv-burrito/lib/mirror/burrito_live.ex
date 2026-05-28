defmodule Mirror.BurritoLive do
  use LocalLiveView.Mirror

  @impl true
  def handle_sync(local_assigns, _mirror_assigns, %{llv_id: llv_id}) do
    Phoenix.PubSub.broadcast(
      Burrito.PubSub,
      "llv_mirror:BurritoLive:#{llv_id}",
      {:llv_attrs, local_assigns}
    )

    {:ok, local_assigns}
  end
end
