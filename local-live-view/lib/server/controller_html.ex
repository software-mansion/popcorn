defmodule LocalLiveView.ControllerHTML do
  @moduledoc """
  HTML rendering module for LocalLiveView controller.

  This module is used internally by LocalLiveView.Controller to render
  the LocalLiveView mount point.
  """

  use Phoenix.Component

  import LocalLiveView.Component

  def index(assigns) do
    ~H"""
    <.local_live_view view={@view} />
    """
  end
end
