defmodule LocalLiveView.E2E.LayoutLive do
  # A regular LiveView rendered from the root layout, see LocalLiveView.E2E.Layouts.
  use LocalLiveView.E2E.Web, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :clicks, 0), layout: false}
  end

  @impl true
  def handle_event("inc", _params, socket) do
    {:noreply, update(socket, :clicks, &(&1 + 1))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <button id="layout-live-inc" phx-click="inc">layout live clicks: {@clicks}</button>
    """
  end
end
