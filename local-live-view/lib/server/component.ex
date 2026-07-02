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

  @doc ~S'''
  Renders a `LocalLiveView` mount point.

  ## Attributes

    * `view` (required) - the LocalLiveView module name, as a string.
    * `id` - stable element id; defaults to a server-generated random id.
    * `endpoint` - the Phoenix endpoint module used to sign the mirror token.

  ## Examples

      <.local_live_view view="MyLocal" />
  '''
  def local_live_view(assigns) do
    view = assigns[:view]

    unless is_binary(view) do
      raise ArgumentError, """
      <.local_live_view> expects view="..." parameter to be a string, got:
        #{inspect(view)}
      """
    end

    assigns =
      assigns
      |> assign_new(:id, fn -> default_id(view) end)
      |> assign_new(:mirror_token, fn ->
        if mirror_exists?(view),
          do: LocalLiveView.MirrorToken.sign(assigns.endpoint, view, assigns.id)
      end)

    ~H"""
    <div
      data-pop-view={@view}
      id={@id}
      phx-hook="LocalLiveView"
      data-pop-mirror-token={@mirror_token}
      phx-update="ignore"
    >
    </div>
    """
  end

  # The mount point lives under `phx-update="ignore"`, so its id MUST be stable
  # across the host LiveView's dead and connected renders — otherwise morphdom
  # rebuilds the element on connect and the serialized assigns are lost before
  # the runtime reads them. A random id would differ between renders, so derive a
  # deterministic one from the module name. Pass an explicit `id` when mounting
  # more than one instance of the same module on a page.
  defp default_id(name), do: "llv-" <> String.replace(name, ~r/[^A-Za-z0-9_-]/, "-")

  defp mirror_exists?(view_name) do
    mirror = String.to_existing_atom("Elixir.Mirror." <> view_name)
    Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 3)
  rescue
    ArgumentError -> false
  end
end
