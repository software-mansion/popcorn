defmodule LocalLiveView.E2E.PageHTML do
  use LocalLiveView.E2E.Web, :html

  # No LiveView and no main local view: nothing owns the URL.
  def plain(assigns) do
    ~H"""
    <p id="plain-page">plain page</p>
    <.local_live_view view="Patcher" id="plain-patcher" to="/plain?x=1" />
    """
  end

  def ssr(assigns) do
    ~H"""
    <.local_live_view view="Echo" id="echo-ssr" label="rendered on the server" />
    <.local_live_view
      view="Echo"
      id="echo-no-ssr"
      label="not rendered on the server"
      llv_ssr={false}
    />
    <%!-- LocalLiveView keeps its own settings for a main view, like its URL,
      apart from the assigns: set by the user, main and url are plain assigns. --%>
    <.local_live_view view="Echo" id="echo-attrs" label="user attrs" main="user main" url="user url" />
    """
  end
end
