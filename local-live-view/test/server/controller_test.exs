defmodule LocalLiveView.ControllerTest do
  use ExUnit.Case, async: true

  defmodule Hello do
    use LocalLiveView

    @impl true
    def handle_params(params, url, socket) do
      {:noreply, assign(socket, tab: params["tab"], url: url)}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <p>{@tab} at {@url}</p>
      """
    end
  end

  test "live_local pages render the view on the server with the request URL" do
    conn =
      Plug.Test.conn(:get, "/hello?tab=news")
      |> Plug.Conn.put_private(:llv_view, inspect(Hello))
      |> Phoenix.Controller.put_format("html")
      |> LocalLiveView.Controller.call(:index)

    assert conn.status == 200
    assert conn.resp_body =~ ~s(data-pop-view="#{inspect(Hello)}")
    assert conn.resp_body =~ "<p>news at http://www.example.com/hello?tab=news</p>"
  end
end
