defmodule LocalLiveView.ControllerHTML do
  # Template renderer for `LocalLiveView.Controller`.
  @moduledoc false

  use Phoenix.Component

  import LocalLiveView.Component

  def index(assigns) do
    ~H"""
    <.local_live_view view={@view} />
    """
  end
end
