defmodule LocalLvCheckoutWeb.PageController do
  use LocalLvCheckoutWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
