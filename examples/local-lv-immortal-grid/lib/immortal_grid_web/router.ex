defmodule ImmortalGridWeb.Router do
  use ImmortalGridWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {ImmortalGridWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :add_headers
  end

  def add_headers(conn, _opts) do
    Plug.Conn.merge_resp_headers(conn, [
      {"Access-Control-Allow-Origin", "*"},
      {"Cross-Origin-Opener-Policy", "same-origin"},
      {"Cache-Control", "public no-cache"},
      {"Cross-Origin-Embedder-Policy", "require-corp"}
    ])
  end

  scope "/", ImmortalGridWeb do
    pipe_through :browser

    get "/", PageController, :home
    live "/presenter", PresenterLive
    live "/play", PlayLive
  end

  if Application.compile_env(:immortal_grid, :dev_routes) do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: ImmortalGridWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
