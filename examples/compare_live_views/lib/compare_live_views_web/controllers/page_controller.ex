defmodule CompareLiveViewsWeb.PageController do
  use CompareLiveViewsWeb, :controller

  def home(conn, _params) do
    conn
    |> render(:home)
  end
end
