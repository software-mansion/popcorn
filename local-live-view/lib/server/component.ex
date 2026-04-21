defmodule LocalLiveView.Component do
  @moduledoc """
  Phoenix component for mounting a LocalLiveView.

  Import this module in your application's CoreComponents:

      defmodule MyAppWeb.CoreComponents do
        import LocalLiveView.Component
        ...
      end

  Then use it in your templates:

      <.local_live_view view="MyLive" />
  """
  use Phoenix.Component

  @doc """
  Renders a LocalLiveView mount point.

  ## Examples

      <.local_live_view view="MyLive" />

  """
  attr(:view, :string, required: true)
  attr(:id, :string, doc: "stable element id, defaults to view name")

  def local_live_view(assigns) do
    assigns = assign_new(assigns, :id, fn -> assigns.view end)
    assigns = assign(assigns, :has_mirror, mirror_exists?(assigns.view))

    ~H"""
    <div
      data-pop-view={@view}
      id={@id}
      data-pop-mirror={@has_mirror || nil}
      phx-update="ignore"
    >
    </div>
    """
  end

  defp mirror_exists?(view_name) do
    mirror = Module.concat(Mirror, String.to_atom(view_name))
    Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 2)
  end
end
