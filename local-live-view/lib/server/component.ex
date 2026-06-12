defmodule LocalLiveView.Component do
  @moduledoc """
  Phoenix components for mounting a `LocalLiveView` or a `LocalComponent`.

  Import this module in your application's CoreComponents:

      defmodule MyAppWeb.CoreComponents do
        import LocalLiveView.Component
        ...
      end

  Then use them in your templates:

      <.local_live_view view="MyLive" />
      <.local_component module="MyComponent" items={@items} />
  """
  use Phoenix.Component

  @doc ~S'''
  Renders a `LocalLiveView` mount point.

  ## Attributes

    * `view` (required) - the LocalLiveView module name, as a string.
    * `id` - stable element id; defaults to a server-generated random id.

  ## Examples

      <.local_live_view view="MyLocal" />

  To seed a local view with server-side state, use a `LocalComponent` and
  `local_component/1` instead — local views do not take parameters.
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
      |> assign(:has_mirror, mirror_exists?(view))

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

  @doc ~S'''
  Renders a `LocalComponent` mount point.

  Like `Phoenix.Component.live_component/1`, any attribute other than `module` is
  forwarded to the component as the assigns of its `c:LocalComponent.update/2`
  callback. `id` is forwarded too, and additionally used as the mount point's DOM
  id. Assigns are serialized as JSON and arrive with atom keys at the top level
  (nested maps keep string keys, as they cross the JSON boundary).

  ## Attributes

    * `module` (required) - the LocalComponent module name, as a string.
    * `id` - stable element id; defaults to a server-generated random id.

  ## Examples

      <.local_component module="Cart" />

      <.local_component module="Cart" items={@items} currency="EUR" />

  The second example mounts the component, calling its `update/2` with:

      %{id: _, items: items, currency: "EUR"}
  '''
  def local_component(assigns) do
    module = assigns[:module]

    unless is_binary(module) do
      raise ArgumentError, """
      <.local_component> expects module="..." parameter to be a string, got:
        #{inspect(module)}
      """
    end

    if Map.has_key?(assigns, :inner_block) do
      raise ArgumentError, "<.local_component> does not accept inner content"
    end

    assigns = assign_new(assigns, :id, fn -> default_id(module) end)

    # Every attribute except `module` is forwarded as an assign; `__changed__` is
    # Phoenix's internal change-tracking map and is always dropped.
    comp_assigns = Map.drop(assigns, [:__changed__, :module])

    assigns =
      assigns
      |> assign(:has_mirror, mirror_exists?(module))
      |> assign(:comp_assigns, comp_assigns)

    ~H"""
    <div
      data-pop-view={@module}
      id={@id}
      data-pop-mirror={@has_mirror || nil}
      data-pop-assigns={encode_assigns(@comp_assigns)}
      phx-update="ignore"
    >
    </div>
    """
  end

  defp encode_assigns(assigns) when assigns == %{}, do: nil
  defp encode_assigns(assigns), do: Jason.encode!(assigns)

  # The mount point lives under `phx-update="ignore"`, so its id MUST be stable
  # across the host LiveView's dead and connected renders — otherwise morphdom
  # rebuilds the element on connect and the serialized assigns are lost before
  # the runtime reads them. A random id would differ between renders, so derive a
  # deterministic one from the module name. Pass an explicit `id` when mounting
  # more than one instance of the same module on a page.
  defp default_id(name), do: "llv-" <> String.replace(name, ~r/[^A-Za-z0-9_-]/, "-")

  defp mirror_exists?(view_name) do
    mirror = Module.concat(Mirror, String.to_atom(view_name))
    Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 3)
  end
end
