defmodule Mirror.Mirrored do
  # The server-side mirror of the Mirrored local view.
  use LocalLiveView.Mirror

  @impl true
  def handle_sync(local_assigns, _mirror_assigns, %{mirror_id: mirror_id}) do
    Phoenix.PubSub.broadcast(
      LocalLiveView.E2E.PubSub,
      "mirror:" <> mirror_id,
      {:synced, local_assigns}
    )

    {:ok, local_assigns}
  end
end
