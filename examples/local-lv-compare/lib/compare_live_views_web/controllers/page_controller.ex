defmodule CompareLiveViewsWeb.PageController do
  use CompareLiveViewsWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
