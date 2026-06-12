defmodule LocalComponent do
  @moduledoc ~S'''
  LocalComponent is the `Phoenix.LiveComponent` counterpart of `LocalLiveView`:
  it runs inside the browser in the Popcorn runtime, but exposes the lifecycle of
  a stateful component and can be seeded with assigns from the server.

  Use it exactly like a `Phoenix.LiveComponent`, with `mount/1`, `update/2`,
  `render/1` and `handle_event/3`:

      defmodule Cart do
        use LocalComponent

        @impl true
        def mount(socket) do
          {:ok, assign(socket, open: false)}
        end

        @impl true
        def update(%{items: items}, socket) do
          {:ok, assign(socket, :items, items)}
        end

        @impl true
        def render(assigns) do
          ~H"""
          <div>{length(@items)} items</div>
          """
        end
      end

  Mount it on a page (typically from a server `LiveView`) with the
  `LocalLiveView.Component.local_component/1` component, passing assigns inline:

      <.local_component module="Cart" items={@items} />

  On mount, `mount/1` is called once, then `update/2` receives the assigns passed
  via the component. Whenever the hosting (server) `LiveView` re-renders with
  changed assigns, `update/2` runs again with the new values — so server-side
  state stays reflected in the local component. Assigns arrive with atom keys at
  the top level; nested maps keep string keys, since they are serialized to JSON
  to cross into the runtime.
  '''

  alias Phoenix.LiveView.Socket

  @callback mount(socket :: Socket.t()) :: {:ok, Socket.t()}
  @callback update(assigns :: map(), socket :: Socket.t()) :: {:ok, Socket.t()}

  @optional_callbacks mount: 1, update: 2

  defmacro __using__(opts) do
    quote do
      use LocalLiveView, unquote(opts)
      @behaviour LocalComponent

      # Bridge the LocalLiveView mount/3 onto the component lifecycle: call
      # mount/1 once, then feed the assigns delivered by the component through
      # update/2, mirroring how Phoenix drives a LiveComponent.
      @impl true
      def mount(assigns, _session, socket) do
        case mount(socket) do
          {:ok, %Phoenix.LiveView.Socket{} = socket} ->
            update(LocalComponent.__normalize_assigns__(assigns), socket)

          other ->
            other
        end
      end

      @impl true
      def mount(socket), do: {:ok, socket}

      @impl true
      def update(assigns, socket), do: {:ok, assign(socket, assigns)}

      defoverridable mount: 1, update: 2

      @doc false
      # Internal entrypoint the server calls when the host LiveView pushes new
      # assigns. Not overridable, so it always reaches the user's update/2.
      def __llv_update__(assigns, socket) do
        update(LocalComponent.__normalize_assigns__(assigns), socket)
      end
    end
  end

  @doc false
  # Top-level assign keys cross the JSON boundary as strings; convert them back to
  # atoms so they read like Phoenix assigns (`@items`). Nested values are left as
  # is — deeply atomizing arbitrary maps would be unsafe.
  def __normalize_assigns__(assigns) do
    Map.new(assigns, fn
      {key, value} when is_atom(key) -> {key, value}
      {key, value} when is_binary(key) -> {String.to_atom(key), value}
    end)
  end
end
