defmodule LocalLiveView.Component do
  @moduledoc """
  Phoenix component for mounting a `LocalLiveView`.

  Import this module in your application's CoreComponents:

      defmodule MyAppWeb.CoreComponents do
        import LocalLiveView.Component
        ...
      end

  Then use it in your templates:

      <.local_live_view view="MyLive" />
      <.local_live_view view="Cart" items={@items} />
  """
  use Phoenix.Component

  @doc ~S'''
  Renders a `LocalLiveView` mount point.

  Like `Phoenix.Component.live_component/1`, any attribute other than `view` is
  forwarded to the view as the assigns of its `c:LocalLiveView.update/2`
  callback. `id` is forwarded too, and additionally used as the mount point's DOM
  id. Assigns are serialized as JSON and arrive with atom keys at the top level
  (nested maps keep string keys, as they cross the JSON boundary).

  ## Attributes

    * `view` (required) - the LocalLiveView module name, as a string.
    * `id` - stable element id; defaults to a server-generated random id.
    * `endpoint` - the Phoenix endpoint module used to sign the mirror token.

  ## Examples

      <.local_live_view view="MyLocal" />

      <.local_live_view view="Cart" items={@items} currency="EUR" />

  The second example mounts the view, calling its `update/2` with:

      %{id: _, items: items, currency: "EUR"}
  '''
  def local_live_view(assigns) do
    view = assigns[:view]

    unless is_binary(view) do
      raise ArgumentError, """
      <.local_live_view> expects view="..." parameter to be a string, got:
        #{inspect(view)}
      """
    end

    if mirror_exists?(view) && !assigns[:endpoint] do
      raise ArgumentError, """
      <.local_live_view> with mirror mechanism requires endpoint="..." parameter.
      View "#{view}" uses mirror, but no endpoint was provided.
      Add endpoint to your component call:
        <.local_live_view view="#{view}" endpoint={@endpoint} />
      """
    end

    if Map.has_key?(assigns, :inner_block) do
      raise ArgumentError, "<.local_live_view> does not accept inner content"
    end

    assigns = assign_new(assigns, :id, fn -> default_id(view) end)

    comp_assigns = Map.drop(assigns, [:__changed__, :view])

    mirror_token =
      if mirror_exists?(view),
        do: LocalLiveView.MirrorToken.sign(assigns.endpoint, view, assigns.id)

    assigns = assign(assigns, mirror_token: mirror_token, comp_assigns: comp_assigns)

    ~H"""
    <div
      data-pop-view={@view}
      id={@id}
      phx-hook="LocalLiveView"
      data-pop-mirror-token={@mirror_token}
      data-pop-assigns={encode_assigns(@comp_assigns)}
      phx-update="ignore"
    >
    </div>
    <%!-- Event bus for local→server pushes (push_server_event): the browser
          runtime sends them through this element's hook, whose mounted /
          destroyed lifecycle tracks the host LiveView across reconnects and
          re-renders. A sibling (not a child) of the mount point, so the host
          LiveView owns it. Renders inert on pages without a host. --%>
    <div id={"#{@id}-llv-event-bus"} data-llv-event-bus-for={@id} phx-hook="LocalLiveViewEventBus" hidden>
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

  defp mirror_exists?(view_name), do: LocalLiveView.Mirror.find_module(view_name) != nil
end
