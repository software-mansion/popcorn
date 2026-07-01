defmodule LocalLiveView.Popconent do
  @moduledoc ~S'''
  LocalLiveView.Popconent is a component running on the client,
  but spawned by a server Live View (directly or via its child
  component or live component).

  The Popconent's API is the same as of `Phoenix.LiveComponent`, with `mount/1`, `update/2`,
  `render/1` and `handle_event/3`:

      defmodule Cart do
        use LocalLiveView.Popconent

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

  It can be mounted it with the `LocalLiveView.Component.popconent/1` component,
  passing assigns inline:

      <.popconent module="Cart" items={@items} />

  Then, the popconent behaves like a live component: it can receive events
  and update is called whenever the assigns change. However, there are some
  key differences from the live component:

  - Popconent lives in a separate process from the parent live view - that's
  because they live in different runtimes.
  - Events sent by popconent's child components land in the popconent by default,
  not in its parent live view.

  However, it's possible to send events to the parent live view, by using
  `phx-target={@server}`, for example:

      <button phx-click="archive" phx-target={@server}>Archive</button>

  or by calling `push_server_event/3`.

  Events can be sent to multiple targets with `targets/1`.
  For example, to send an event to the parent popconent _and_ the server,
  set the target to `targets([@default, @server])`:

      <button phx-click="archive" phx-target={targets([@default, @server]}>Archive</button>

  This approach is useful for optimistic updates. The event is first
  handled locally by the popconent, then it's also handled on the server.
  Each of them updates popconent's assigns: the local update is optimistic,
  then the server update can possibly override it.
  '''

  alias Phoenix.LiveView.Socket

  @callback mount(socket :: Socket.t()) :: {:ok, Socket.t()}
  @callback update(assigns :: map(), socket :: Socket.t()) :: {:ok, Socket.t()}
  @callback render(assigns :: Socket.assigns()) :: Phoenix.LiveView.Rendered.t()
  @callback handle_event(event :: binary(), params :: map(), socket :: Socket.t()) ::
              {:noreply, Socket.t()} | {:reply, map(), Socket.t()}
  @callback handle_info(msg :: term(), socket :: Socket.t()) :: {:noreply, Socket.t()}

  @optional_callbacks mount: 1, update: 2, handle_event: 3, handle_info: 2

  defmacro __using__(_opts) do
    quote do
      @behaviour LocalLiveView.Popconent

      # A popconent is a client-side live view, so it needs Phoenix.Component (for
      # ~H / assign) and render compilation — but not the mirror-channel machinery
      # `use LocalLiveView` injects (handle_server_event, llv_server_message), which
      # a popconent never uses. Set up only what's needed here.
      import LocalLiveView.Popconent, only: [targets: 1, push_server_event: 3]
      @before_compile Phoenix.LiveView.Renderer
      use Phoenix.Component, global_prefixes: ~w(pop-)

      # The runtime reads __live__/0. A popconent takes no on_mount hooks or view
      # opts, so it's just LocalLiveView's default map — no @before_compile
      # attribute-accumulation machinery needed.
      @doc false
      def __live__, do: LocalLiveView.__live__()

      # Marker so LocalLiveView.Server can tell a popconent from a full local live
      # view and drive it via mount!/3 + update/3. A function (not @behaviour
      # reflection) because that's all the AtomVM runtime the server runs in exposes.
      @doc false
      def __popconent__, do: true

      # Component lifecycle, mirroring how Phoenix drives a LiveComponent. Only the
      # defaults are injected; LocalLiveView.Server drives the mount/1 + update/2
      # lifecycle itself (detecting a popconent by the __popconent__/0 marker), so
      # there's no mount/3 or __llv_update__/2 to implement here.
      @impl true
      def mount(socket), do: {:ok, socket}

      @impl true
      def update(assigns, socket), do: {:ok, assign(socket, assigns)}

      @impl true
      def handle_info(_msg, socket), do: {:noreply, socket}

      defoverridable mount: 1, update: 2, handle_info: 2
    end
  end

  # Delimiter joining composed phx-target tokens (see targets/1). ASCII Unit
  # Separator — it never appears in a CSS selector or cid, so the JS can split it
  # back unambiguously. Keep in sync with LLV_TARGET_SEP in assets/local_live_view.js.
  @target_sep "\x1f"

  @doc """
  Composes a `phx-target` from a list of targets.

  Combine the `@default` (local, as if no target) and `@server` (host LiveView)
  assigns with each other and/or ordinary `phx-target` selectors/cids, dispatching
  the event to every target in the list:

      <button phx-click="save" phx-target={targets([@default, @server])}>Save</button>
      <button phx-click="ping" phx-target={targets([@default, @server, "#cart"])}>Ping</button>

  A single target needs no helper — `phx-target={@server}` works on its own.
  """
  def targets(list) when is_list(list), do: Enum.map_join(list, @target_sep, &to_string/1)

  @doc """
  Sends an event to the host (server) `LiveView` that mounts this popconent.

  The programmatic counterpart of `phx-target={@server}`: callable from a
  popconent's `handle_event/3` or `handle_info/2` (so the payload can be computed
  — e.g. a drag result), it delivers `event`/`payload` to the host LiveView's
  `handle_event(event, payload, socket)` over the regular Phoenix websocket.
  """
  def push_server_event(%Socket{} = socket, event, payload \\ %{}) do
    Popcorn.Wasm.run_js(
      """
      ({ args }) => {
        if (window.__llvPushServer) {
          window.__llvPushServer(args.id, args.event, args.payload);
        }
      }
      """,
      %{
        id: socket.private[:llv_id],
        event: to_string(event),
        payload: LocalLiveView.Utils.to_serializable(payload)
      }
    )

    socket
  end
end
