defmodule LlvIntegrationWeb.PageController do
  use LlvIntegrationWeb, :controller

  def plain(conn, _params), do: render(conn, :plain)

  def ssr(conn, _params), do: render(conn, :ssr)
end
