defmodule LlvIntegrationWeb.MirroredHostLive do
  # Hosts the Mirrored local view and shows what its mirror received.
  use LlvIntegrationWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      mirror_id = LocalLiveView.Component.mirror_id(socket, "mirrored")
      Phoenix.PubSub.subscribe(LlvIntegration.PubSub, "mirror:" <> mirror_id)
    end

    {:ok, assign(socket, :synced, nil)}
  end

  @impl true
  def handle_info({:synced, local_assigns}, socket) do
    {:noreply, assign(socket, :synced, local_assigns["clicks"] || local_assigns[:clicks])}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <p id="mirror-synced">synced: {@synced}</p>
    <.local_live_view view="Mirrored" id="mirrored" />
    """
  end
end
