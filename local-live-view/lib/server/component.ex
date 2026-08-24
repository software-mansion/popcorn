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
  callback. `id` is forwarded too, and additionally used as the view's DOM id
  (the id of the root div inside the mount point, which becomes the LiveView
  container at runtime).

  ## Attributes

    * `view` (required) - the LocalLiveView module name, as a string.
    * `id` - stable element id; defaults to a server-generated random id.

  ## Examples

      <.local_live_view view="MyLocal" />

      <.local_live_view view="Cart" items={@items} currency="EUR" />

  The second example mounts the view, calling its `update/2` with:

      %{id: _, items: items, currency: "EUR"}
  '''
  def local_live_view(assigns) do
    view = assigns[:view]
    id = assigns[:id] || default_id(view)

    assigns = assign(assigns, id: id)

    if mirror_exists?(view) do
      ~H"""
      <.live_component module={__MODULE__.Mirrored} {assigns} />
      """
    else
      render_static(assigns)
    end
  end

  defp render_static(assigns) do
    comp_assigns = comp_assigns(assigns)

    assigns =
      assign(assigns,
        mirror_token: nil,
        mirror_id: nil,
        comp_assigns: comp_assigns
      )

    render_markup(assigns)
  end

  defmodule Mirrored do
    @moduledoc false
    use Phoenix.LiveComponent
    alias LocalLiveView.Component, as: LLVComponent

    @impl true
    def update(assigns, socket) do
      view = assigns[:view]

      mirror_id =
        case socket.assigns[:mirror_id] do
          nil -> LLVComponent.mirror_id(socket, assigns.id)
          mirror_id -> mirror_id
        end

      mirror_token =
        cond do
          token = socket.assigns[:mirror_token] ->
            token

          Phoenix.LiveView.connected?(socket) ->
            endpoint = socket.endpoint || LLVComponent.resolve_default_endpoint()
            LocalLiveView.MirrorToken.sign(endpoint, view, mirror_id)

          true ->
            nil
        end

      comp_assings = LLVComponent.comp_assigns(assigns)

      socket =
        assign(socket,
          view: view,
          id: assigns.id,
          mirror_token: mirror_token,
          mirror_id: mirror_id,
          comp_assigns: comp_assings
        )

      {:ok, socket}
    end

    @impl true
    def render(assigns) do
      LLVComponent.render_markup(assigns)
    end
  end

  @doc false
  defp validate_assigns!(assigns) do
    view = assigns[:view]

    unless is_binary(view) do
      raise ArgumentError, """
      <.local_live_view> expects view="..." parameter to be a string, got:
        #{inspect(view)}
      """
    end

    if Map.has_key?(assigns, :inner_block) do
      raise ArgumentError, "<.local_live_view> does not accept inner content"
    end
  end

  @doc false
  def render_markup(assigns) do
    validate_assigns!(assigns)

    ~H"""
    <%!-- The mount point: one host-owned element carrying the LocalLiveView
    hook (which drives the view's lifecycle from the host LiveView), the
    view identity (data-pop-view, data-pop-id) and the host→view data
    attributes (assigns, mirror config). phx-update="ignore" keeps host
    patches away from the subtree below — documented LiveView semantics —
    while the ignored element itself still gets its data-* attributes
    merged and its hook's updated() fired on every patch, so assign
    updates keep flowing. The hook nested in the ignored subtree (the
    event bus) mounts from the initial HTML and never needs patching. --%>
    <div
      id={"#{@id}-llv-hook"}
      phx-hook="LocalLiveView"
      phx-update="ignore"
      data-pop-view={@view}
      data-pop-id={@id}
      data-pop-assigns={encode_assigns(@comp_assigns)}
      data-pop-mirror-token={@mirror_token}
      data-pop-mirror-id={@mirror_id}
    >
    <%!-- The LLV root placeholder. On "create" the WASM runtime renders the
    view's real LiveView container — a sticky root whose data-phx-session
    token is signed by LiveView's own machinery — and the LLVEngine
    replaces this div with it, then joins it (through the patched
    newRootView, which puts its channel on the popcorn socket). The
    canonical view id (@id) belongs to the container (it is the LiveView's
    id, its topic, and the id every LLV API is keyed on); the mount point
    carries it in data-pop-id. --%>
    <div id={@id} data-pop-root></div>
    <%!-- Stub for sending events from client to server. See LLVEngine class.
    Deliberately its own empty element: pushing through a hook freezes the
    hook's element and all descendants until LiveView acks. --%>
    <div id={"#{@id}-llv-event-bus"} data-llv-event-bus-for={@id} phx-hook="LocalLiveViewEventBus" hidden>
    </div>
    </div>
    """
  end

  @doc false
  def comp_assigns(assigns) do
    Map.drop(assigns, [:__changed__, :view])
  end

  defp encode_assigns(assigns), do: Base.encode64(:erlang.term_to_binary(assigns))

  @doc false
  def resolve_default_endpoint do
    Application.get_env(:local_live_view, :default_endpoint)
  end

  @doc """
  Assembles the deterministic `mirror_id` for a given LocalLiveView component
  within a parent LiveView process.

  Accepts the parent `socket` (or directly its `socket.id`), and either:
  * a default ID derived from view name
  * an explicit custom DOM ID provided to `<.local_live_view id="..." />`

  ## Examples

      # Explicit ID passed to component:
      mirror_id = LocalLiveView.Component.mirror_id(socket, "my-custom-cart")

      # Default ID derived from view name:
      mirror_id = LocalLiveView.Component.mirror_id(socket, "llv-Cart")
  """
  @spec mirror_id(Phoenix.LiveView.Socket.t(), String.t()) :: String.t()
  def mirror_id(%Phoenix.LiveView.Socket{id: socket_id}, id) do
    mirror_id(socket_id, id)
  end

  def mirror_id(socket_id, id) when is_binary(socket_id) and is_binary(id) do
    socket_id <> "-" <> id
  end

  defp default_id(name) when is_binary(name) do
    "llv-" <> String.replace(name, ~r/[^A-Za-z0-9_-]/, "-")
  end

  defp mirror_exists?(view_name), do: LocalLiveView.Mirror.find_module(view_name) != nil
end
