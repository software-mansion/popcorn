defmodule ImmortalGridWeb.PageController do
  use ImmortalGridWeb, :controller

  def home(conn, _params) do
    redirect(conn, to: ~p"/play")
  end
end
