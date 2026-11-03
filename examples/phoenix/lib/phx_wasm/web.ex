defmodule PhxWasm.Router do
  use Phoenix.Router
  import Phoenix.LiveView.Router

  pipeline :browser do
    plug :accepts, ["html"]
    plug :fetch_session
    plug :fetch_live_flash
    plug :protect_from_forgery
    plug :put_secure_browser_headers
  end

  scope "/", PhxWasm do
    pipe_through :browser

    get "/", PageController, :home
    post "/echo", PageController, :echo
    get "/redirect", PageController, :redirect_home
    live "/counter", CounterLive
  end
end

defmodule PhxWasm.PageController do
  use Phoenix.Controller, formats: [:html]

  def home(conn, _params) do
    visits = (get_session(conn, :visits) || 0) + 1

    conn
    |> put_session(:visits, visits)
    |> html("""
    <p id="visits">visits: #{visits}</p>
    <form method="post" action="/echo">
      <input type="hidden" name="_csrf_token" value="#{get_csrf_token()}">
      <input name="msg" value="hi">
    </form>
    """)
  end

  def echo(conn, %{"msg" => msg}), do: text(conn, "echo: #{msg}")

  def redirect_home(conn, _params), do: redirect(conn, to: "/")
end

defmodule PhxWasm.CounterLive do
  use Phoenix.LiveView

  def mount(_params, session, socket) do
    {:ok, assign(socket, count: 0, visits: session["visits"], connected: connected?(socket))}
  end

  def render(assigns) do
    ~H"""
    <p id="count">count: {@count}</p>
    <p id="lv-visits">visits in session: {@visits}</p>
    <p id="connected">connected: {@connected}</p>
    <button id="inc" phx-click="inc">+</button>
    """
  end

  def handle_event("inc", _params, socket), do: {:noreply, update(socket, :count, &(&1 + 1))}
end

defmodule PhxWasm.ErrorHTML do
  def render(template, _assigns), do: Phoenix.Controller.status_message_from_template(template)
end
