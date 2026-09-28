defmodule Mirrored do
  # A view with a server-side mirror, Mirror.Mirrored.
  use LocalLiveView

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :clicks, 0)}
  end

  @impl true
  def handle_event("click", _params, socket) do
    socket = update(socket, :clicks, &(&1 + 1))
    LocalLiveView.mirror_sync(socket, [:clicks])
    {:noreply, socket}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <button id="mirrored-click" phx-click="click">mirrored clicks: {@clicks}</button>
    """
  end
end
