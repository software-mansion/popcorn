defmodule FormDemoWeb.Router do
  use FormDemoWeb, :router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {FormDemoWeb.Layouts, :root}
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
        {"Cache-Control", "public no-cache"},
        {"Cross-Origin-Embedder-Policy", "require-corp"}
      ]
    )
  end

  scope "/", FormDemoWeb do
    pipe_through :browser

    get "/", PageController, :home
  end

  # Other scopes may use custom stacks.
  # scope "/api", FormDemoWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:form_demo, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: FormDemoWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
