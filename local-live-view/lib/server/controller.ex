defmodule LocalLiveView.Controller do
  # Renders a LocalLiveView mount point for a route declared with
  # `LocalLiveView.Router.live_local/2`. The view module name is carried in
  # `conn.private.llv_view` and handed to the template renderer.
  @moduledoc false

  use Phoenix.Controller, formats: [html: "ControllerHTML"]

  @doc false
  def index(conn, _params) do
    render(conn, :index, view: conn.private.llv_view)
  end
end
