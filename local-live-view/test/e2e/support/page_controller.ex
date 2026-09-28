defmodule LocalLiveView.E2E.PageController do
  use LocalLiveView.E2E.Web, :controller

  def plain(conn, _params), do: render(conn, :plain)

  def ssr(conn, _params), do: render(conn, :ssr)
end
