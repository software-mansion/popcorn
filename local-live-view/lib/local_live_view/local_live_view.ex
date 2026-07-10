defmodule LocalLiveView do
  @moduledoc ~S'''
  `LocalLiveView` implements functionality of `Phoenix.LiveView`
  inside the browser in Popcorn runtime.

  Instead of running the code on the server and sending
  updates via WebSocket, the local live view's code is sent
  to the browser on page load. Whenever you render a local
  live view on the page, it is run on the client.

  `LocalLiveView` API similar to `Phoenix.LiveView`:
  - it runs in a separate Elixir process,
  - it has `c:mount/3` and `c:render/1` callbacks, which behave
    the same way as in a regular live view,
  - it can spawn regular `Phoenix.Component`s and `Phoenix.LiveComponent`s,
  - events from these components by default go to their parent
    local live view.

  Thus, you can implement a simple local live view just like a regular live view:

  ```
  defmodule DemoLive do
    use LocalLiveView

    def render(assigns) do
      ~H"""
      Hello world!
      """
    end
  end
  ```

  and add it to your page using the `local_live_view/1` component:

  ```
  <.local_live_view view="DemoLive" />
  ```

  The main difference between `LocalLiveView` and `Phoenix.LiveView` is that
  the former can also accept assigns and has an `update` callback, like
  `Phoenix.LiveComponent`:

  ```
  defmodule Cart do
    use LocalLiveView

    @impl true
    def mount(_params, _session, socket) do
      {:ok, assign(socket, open: false)}
    end

    @impl true
    def update(%{items: items}, socket) do
      {:ok, assign(socket, :items, items)}
    end

    @impl true
    def render(assigns) do
      ~H"""
      <div>The cart has {length(@items)} items</div>
      """
    end
  end
  ```

  You can reder it the following way:

  ```
  <.local_live_view view="Cart" items={@items} />
  ```

  ## Sending events to the server

  When a local live view is rendered inside a `Phoenix.LiveView`,
  it's possible to send events to that server-side live view, by using
  `phx-target={@server}`, for example:

  ```
  <button phx-click="archive" phx-target={@server}>Archive</button>
  ```

  or by calling `push_server_event/3`.

  Events can be sent to multiple targets with `targets/1`.
  For example, to send an event to the LocalLiveView itself _and_ the server,
  set the target to `targets([@default, @server])`:

  ```
  <button phx-click="archive" phx-target={targets([@default, @server])}>Archive</button>
  ```

  This approach is useful for optimistic updates. The event is first
  handled locally, then it's also handled on the server. Each of them
  updates the LocalLiveView's assigns: the local update is optimistic,
  then the server update can possibly override it.

  ## Handling failed server pushes

  A `push_server_event/3` can fail: the page may have no host LiveView,
  the websocket may be disconnected, or the host may reply with an error
  or time out. When that happens, the view's `c:handle_push_error/4`
  callback is invoked with the event, its params and `server_assigns` —
  the last value of each assign received from the host server. The
  default implementation feeds `server_assigns` through `c:update/2`,
  rolling optimistic local edits back to the latest authoritative state.

  Note this covers only `push_server_event/3`; events dispatched via
  `phx-target={@server}` are not tracked.

  Another way of communicating the server is by using `mirror_sync/2`.
  '''

  alias Phoenix.LiveView.Socket

  @doc """
  Syncs the declared mirror assigns to the server-side mirror channel.
  Must be called from within a LocalLiveView callback (handle_event, handle_info)
  after assigns have been updated.
  """
  def mirror_sync(%Phoenix.LiveView.Socket{} = socket, mirror_keys) do
    payload =
      Map.new(mirror_keys, fn key ->
        {to_string(key), socket.assigns |> Map.get(key) |> LocalLiveView.Utils.to_serializable()}
      end)

    unless payload == %{} do
      Popcorn.Wasm.run_js(
        """
        ({ args }) => {
          if (window.__llvSync) {
            window.__llvSync(args.id, "sync", args.payload);
          }
        }
        """,
        %{id: socket.private[:llv_id], payload: payload}
      )
    end

    socket
  end

  @doc """
  Mimics `Phoenix.LiveView.connected?/1`. Always returns `true`.

  Helps code reusability between server and local LiveViews.
  """
  def connected?(_socket), do: true

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
  Sends an event to the host (server) `LiveView` that mounts this local live view.

  The programmatic counterpart of `phx-target={@server}`: callable from
  `handle_event/3` or `handle_info/2` (so the payload can be computed — e.g. a
  drag result), it delivers `event`/`payload` to the host LiveView's
  `handle_event(event, payload, socket)` over the regular Phoenix websocket.

  If the push fails (no host LiveView, disconnected socket, error reply or
  timeout), the view's `c:handle_push_error/4` callback is invoked.
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

  defmacro __using__(opts) do
    quote bind_quoted: [opts: opts] do
      import LocalLiveView, only: [mirror_sync: 2, targets: 1, push_server_event: 3]
      @behaviour LocalLiveView
      @before_compile Phoenix.LiveView.Renderer
      @phoenix_live_opts []
      Module.register_attribute(__MODULE__, :phoenix_live_mount, accumulate: true)
      @before_compile LocalLiveView
      alias LocalLiveView.Message
      use Phoenix.Component, global_prefixes: ~w(pop-)

      @impl true
      def handle_event("llv_server_message", %{"type" => type} = params, socket) do
        handle_server_event(type, params, socket)
      end

      def handle_server_event(_, _, socket) do
        {:noreply, socket}
      end

      @impl true
      def update(assigns, socket) do
        {:ok, assign(socket, assigns)}
      end

      @impl true
      def handle_info(_msg, socket) do
        {:noreply, socket}
      end

      @impl true
      def handle_push_error(_event, _params, server_assigns, socket) do
        {:ok, socket} = update(server_assigns, socket)
        {:noreply, socket}
      end

      defoverridable handle_server_event: 3, update: 2, handle_info: 2, handle_push_error: 4
    end
  end

  @doc false
  defmacro __before_compile__(env) do
    opts = Module.get_attribute(env.module, :phoenix_live_opts)

    on_mount =
      env.module
      |> Module.get_attribute(:phoenix_live_mount)
      |> Enum.reverse()

    live = LocalLiveView.__live__([on_mount: on_mount] ++ opts)

    quote do
      @doc false
      def __live__ do
        unquote(Macro.escape(live))
      end
    end
  end

  @doc false
  def __live__(opts \\ []) do
    on_mount = opts[:on_mount] || []

    layout =
      Phoenix.LiveView.Utils.normalize_layout(Keyword.get(opts, :layout, false))

    log =
      case Keyword.fetch(opts, :log) do
        {:ok, false} -> false
        {:ok, log} when is_atom(log) -> log
        :error -> :debug
        _ -> raise ArgumentError, ":log expects an atom or false, got: #{inspect(opts[:log])}"
      end

    container = opts[:container] || {:div, []}

    %{
      container: container,
      kind: :view,
      layout: layout,
      lifecycle: Phoenix.LiveView.Lifecycle.build(on_mount),
      log: log
    }
  end

  @doc """
  Navigates to the given path with a browser history push, then calls `handle_params/3`
  with the new URL query params. No server round-trip.

  Mirrors `Phoenix.LiveView.push_patch/2` semantics.

  ## Options

    * `:to` — the path to navigate to (required)
    * `:replace` — when `true`, replaces the current history entry instead of pushing a new one
  """
  def push_patch(%Phoenix.LiveView.Socket{} = socket, opts) when is_list(opts) do
    to = Keyword.fetch!(opts, :to)
    kind = if opts[:replace], do: :replace, else: :push
    %{socket | redirected: {:live, :patch, %{to: to, kind: kind}}}
  end

  @type unsigned_params :: map

  @callback render(assigns :: Socket.assigns()) :: Phoenix.LiveView.Rendered.t()

  @callback mount(
              params :: unsigned_params() | :not_mounted_at_router,
              session :: map,
              socket :: Socket.t()
            ) ::
              {:ok, Socket.t()} | {:ok, Socket.t(), keyword()}

  @callback handle_params(
              params :: unsigned_params(),
              uri :: String.t(),
              socket :: Socket.t()
            ) :: {:noreply, Socket.t()}

  @callback update(assigns :: map(), socket :: Socket.t()) :: {:ok, Socket.t()}

  @callback handle_event(event :: binary, unsigned_params(), socket :: Socket.t()) ::
              {:noreply, Socket.t()} | {:reply, map, Socket.t()}

  @callback handle_info(message :: term, socket :: Socket.t()) :: {:noreply, Socket.t()}

  @doc """
  Invoked when a `push_server_event/3` fails: there is no host LiveView on the
  page, the websocket is disconnected, or the host replies with an error or
  times out. Events dispatched via `phx-target={@server}` are not covered.

  `event` and `params` are the event name and payload the failed push carried
  (`params` has string keys, after a JSON round-trip, like `c:handle_event/3`
  params). `server_assigns` holds the last value of each assign received from
  the host server (through mount and `c:update/2`) — assigns only ever set
  locally are absent.

  The default implementation feeds `server_assigns` through `c:update/2`, as if
  the host had re-sent them — restoring the latest authoritative state through
  the view's usual derivation path. A view whose `c:update/2` skips unchanged
  data (e.g. guarded by a revision counter) should override this callback and
  force its rollback explicitly.
  """
  @callback handle_push_error(
              event :: binary,
              params :: unsigned_params(),
              server_assigns :: map(),
              socket :: Socket.t()
            ) :: {:noreply, Socket.t()}

  @optional_callbacks mount: 3,
                      update: 2,
                      handle_event: 3,
                      handle_info: 2,
                      handle_params: 3,
                      handle_push_error: 4
end
