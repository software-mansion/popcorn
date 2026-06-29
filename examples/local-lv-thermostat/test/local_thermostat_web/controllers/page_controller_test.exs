defmodule LocalThermostatWeb.PageControllerTest do
  use LocalThermostatWeb.ConnCase

  test "GET / renders the LocalLiveView mount points", %{conn: conn} do
    conn = get(conn, ~p"/")
    html = html_response(conn, 200)

    # The server renders mount points; the views themselves boot in the browser via AtomVM.
    assert html =~ ~s(data-pop-view="ThermostatLive")
    assert html =~ ~s(data-pop-view="ClockLive")
  end
end
