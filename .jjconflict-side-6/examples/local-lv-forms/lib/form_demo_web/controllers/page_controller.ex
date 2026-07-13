defmodule FormDemoWeb.PageController do
  use FormDemoWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
