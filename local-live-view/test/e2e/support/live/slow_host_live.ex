defmodule LocalLiveView.E2E.SlowHostLive do
  # Hosts SlowLocal, whose mount in the browser takes a while.
  use LocalLiveView.E2E.Web, :live_view

  @impl true
  def render(assigns) do
    ~H"""
    <p id="slow-host">slow host</p>
    <.local_live_view view="SlowLocal" />
    """
  end
end
