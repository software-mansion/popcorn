defmodule LocalLiveView.E2E.Router do
  use LocalLiveView.E2E.Web, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :protect_from_forgery
    plug :put_root_layout, html: {LocalLiveView.E2E.Layouts, :root}
  end

  # The root layout renders a Patcher local view, outside the page's own
  # views, see LocalLiveView.E2E.Layouts
  pipeline :layout_patcher do
    plug :assign_layout, layout_patcher: true
  end

  # The root layout renders a regular LiveView, LocalLiveView.E2E.LayoutLive
  pipeline :layout_live do
    plug :assign_layout, layout_live: true
  end

  # Pages with no LiveView and no main local view
  scope "/", LocalLiveView.E2E do
    pipe_through :browser

    get "/plain", PageController, :plain
    get "/ssr", PageController, :ssr
  end

  # Pages whose main view is local
  scope "/" do
    pipe_through [:browser, :layout_patcher]

    live_local "/main", MainLocal
    live_local "/slow_main", SlowMain
  end

  scope "/" do
    pipe_through [:browser, :layout_patcher, :layout_live]

    live_local "/main_live", MainLocal
  end

  # LiveView pages, all in one live session
  scope "/", LocalLiveView.E2E do
    pipe_through [:browser, :layout_patcher]

    live "/hosted", HostLive, :one
    live "/hosted2", HostLive, :two
    live "/other", OtherLive
    live "/slow_host", SlowHostLive
    live "/mirrored", MirroredHostLive
  end

  defp assign_layout(conn, assigns) do
    Enum.reduce(assigns, conn, fn {key, value}, conn -> assign(conn, key, value) end)
  end
end
