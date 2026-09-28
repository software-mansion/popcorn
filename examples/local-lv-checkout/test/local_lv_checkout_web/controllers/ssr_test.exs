defmodule LocalLvCheckoutWeb.SSRTest do
  use LocalLvCheckoutWeb.ConnCase

  # The server render follows the URL, so it matches what the browser will
  # show: both views are the main views of their live_local routes, so they
  # get handle_params/3 with the URL.

  test "GET /?step=2 renders the checkout at the payment step", %{conn: conn} do
    html = conn |> get(~p"/?step=2") |> html_response(200)
    assert mount_point(html) =~ "phx-click=\"prev_step\""
  end

  test "GET /plain?page=orders renders the orders tab", %{conn: conn} do
    html = conn |> get(~p"/plain?page=orders") |> html_response(200)
    assert mount_point(html) =~ "No orders yet."
  end

  defp mount_point(html) do
    [_, mount_point] =
      Regex.run(~r/data-pop-root data-pop-ssr inert>(.*?)<div id="[^"]*-llv-event-bus"/s, html)

    mount_point
  end
end
