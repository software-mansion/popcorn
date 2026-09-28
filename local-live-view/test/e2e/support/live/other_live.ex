defmodule LocalLiveView.E2E.OtherLive do
  # A LiveView page with no local views.
  use LocalLiveView.E2E.Web, :live_view

  @impl true
  def render(assigns) do
    ~H"""
    <p id="other-page">other page</p>
    <.link id="other-link-hosted" navigate="/hosted">hosted page</.link>
    """
  end
end
