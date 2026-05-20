defmodule LocalLvPongWeb.PageController do
  use LocalLvPongWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
