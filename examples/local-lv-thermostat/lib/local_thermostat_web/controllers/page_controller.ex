defmodule LocalThermostatWeb.PageController do
  use LocalThermostatWeb, :controller

  def home(conn, _params) do
    render(conn, :home)
  end
end
