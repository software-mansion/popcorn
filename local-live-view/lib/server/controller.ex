defmodule LocalLiveView.Controller do
  @moduledoc """
  Controller for rendering LocalLiveView mounts via router macros.

  This controller is used internally by the `live_local/2` router macro.
  It intercepts the action name (which is the view module name as an atom)
  and passes it to the template renderer.
  """

  use Phoenix.Controller, formats: [html: "ControllerHTML"]

  def index(conn, _params) do
    render(conn, :index, view: conn.private.llv_view)
  end
end
