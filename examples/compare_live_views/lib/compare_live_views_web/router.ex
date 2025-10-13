defmodule CompareLiveViewsWeb.Router do
  use CompareLiveViewsWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {CompareLiveViewsWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
    plug :add_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
    plug :add_headers
  end
  
  def add_headers(conn, _opts) do
    Plug.Conn.merge_resp_headers(
      conn,
      [
        {"Access-Control-Allow-Origin", "*"},
        {"Cross-Origin-Opener-Policy", "same-origin"},
        {"Cross-Origin-Embedder-Policy", "require-corp"}
      ]
    )
  end
  
  scope "/", CompareLiveViewsWeb do
    pipe_through :browser
    get "/", PageController, :home
  end

  # Other scopes may use custom stacks.
  # scope "/api", CompareLiveViewsWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:compare_live_views, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: CompareLiveViewsWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
