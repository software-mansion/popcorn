defmodule Mirror.BurritoLive do
  use LocalLiveView.Mirror

  @impl true
  def handle_sync(local_assigns, _mirror_assigns) do
    Phoenix.PubSub.broadcast(
      Burrito.PubSub,
      "llv_mirror:BurritoLive",
      {:llv_attrs, local_assigns}
    )

    {:ok, local_assigns}
  end
end
