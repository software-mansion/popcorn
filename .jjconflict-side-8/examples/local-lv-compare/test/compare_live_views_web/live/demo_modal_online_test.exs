defmodule CompareLiveViewsWeb.DemoModalOnlineTest do
  use CompareLiveViewsWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  describe "DemoModalOnline (Phoenix LiveView)" do
    test "hides the modal when the close button is clicked", %{conn: conn} do
      {:ok, view, _html} = live_isolated(conn, CompareLiveViewsWeb.DemoModalOnline)

      # The modal is open right after mount.
      assert has_element?(view, ".modal")

      view |> element(".close") |> render_click()

      refute has_element?(view, ".modal")
    end

    test "shows the modal again when the show button is clicked", %{conn: conn} do
      {:ok, view, _html} = live_isolated(conn, CompareLiveViewsWeb.DemoModalOnline)

      # Close it first so we can observe it being re-opened.
      view |> element(".close") |> render_click()
      refute has_element?(view, ".modal")

      view |> element(".show-modal-button") |> render_click()

      assert has_element?(view, ".modal")
    end
  end
end
