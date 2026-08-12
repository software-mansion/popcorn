defmodule LocalLiveView.ControllerHTML do
  # Template renderer for `LocalLiveView.Controller`.
  @moduledoc false

  use Phoenix.Component

  import LocalLiveView.Component

  @doc false
  def index(assigns) do
    ~H"""
    <.local_live_view view={@view} />
    """
  end
end
