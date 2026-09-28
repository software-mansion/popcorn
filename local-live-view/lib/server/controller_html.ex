defmodule LocalLiveView.ControllerHTML do
  # Template renderer for `LocalLiveView.Controller`.
  @moduledoc false

  use Phoenix.Component

  import LocalLiveView.Component

  def index(assigns) do
    ~H"""
    <%!-- The main view of the page, see LocalLiveView.Router.live_local/2 --%>
    <.local_live_view view={@view} __llv__={%{main: true, url: Plug.Conn.request_url(@conn)}} />
    """
  end
end
