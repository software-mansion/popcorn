defmodule LocalLvKanbanWeb.PageController do
  use LocalLvKanbanWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
