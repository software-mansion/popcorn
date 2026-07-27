defmodule LocalLvCheckoutWeb.Router do
  use LocalLvCheckoutWeb, :router

  import LocalLiveView.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :put_root_layout, html: {LocalLvCheckoutWeb.Layouts, :root}
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  pipeline :api do
    plug :accepts, ["json"]
  end

  pipeline :put_plain_layout do
    plug :put_layout, html: {LocalLvCheckoutWeb.Layouts, :plain}
  end

  scope "/", LocalLvCheckoutWeb do
    pipe_through :browser

    live "/", CheckoutLive
  end

  scope "/", LocalLvCheckoutWeb do
    pipe_through :browser
    pipe_through :put_plain_layout

    live_local "/plain", NavTestLive
  end

  # Other scopes may use custom stacks.
  # scope "/api", LocalLvCheckoutWeb do
  #   pipe_through :api
  # end

  # Enable LiveDashboard and Swoosh mailbox preview in development
  if Application.compile_env(:local_lv_checkout, :dev_routes) do
    # If you want to use the LiveDashboard in production, you should put
    # it behind authentication and allow only admins to access it.
    # If your application does not have an admins-only section yet,
    # you can use Plug.BasicAuth to set up some basic authentication
    # as long as you are also using SSL (which you should anyway).
    import Phoenix.LiveDashboard.Router

    scope "/dev" do
      pipe_through :browser

      live_dashboard "/dashboard", metrics: LocalLvCheckoutWeb.Telemetry
      forward "/mailbox", Plug.Swoosh.MailboxPreview
    end
  end
end
