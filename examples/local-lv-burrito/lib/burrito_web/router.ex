defmodule BurritoWeb.Router do
  use BurritoWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {BurritoWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :add_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
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

  scope "/", BurritoWeb do
    pipe_through :browser
    live "/", OrderLive
  end

  scope "/api", BurritoWeb do
    pipe_through :api
    post "/order/sync", OrderController, :sync
  end

  if Application.compile_env(:burrito, :dev_routes) do
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser
      live_dashboard "/dashboard", metrics: BurritoWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
