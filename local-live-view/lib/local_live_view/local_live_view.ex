defmodule LocalLiveView do
  @moduledoc ~S'''
  LocalLiveView is a module that implements functionality of Phoenix.LiveView inside the browser in
  Popcorn runtime.
  LocalLiveView should be used exactly like its Phoenix equivalent:
  ```
  defmodule MyAppWeb.DemoLive do
    use LocalLiveView

    def render(assigns) do
      ~H"""
      Hello world!
      """
    end
  end
  ```
  The LocalLiveView can be added to a page by adding a `local_live_view/1` component
  to your app's `CoreComponents` (see the Getting Started guide) and using it as:
  ```
  <.local_live_view view="DemoLive" />
  ```

  During application runtime, the application creates a process that handles a LocalLiveView's state,
  by storing and modifying its assigns.
  '''

  defmacro __using__(_opts) do
    quote bind_quoted: [opts: []] do
      import LocalLiveView
      @behaviour LocalLiveView
      @before_compile Phoenix.LiveView.Renderer
      @phoenix_live_opts []
      Module.register_attribute(__MODULE__, :phoenix_live_mount, accumulate: true)
      @before_compile LocalLiveView

      alias LocalLiveView.Message
      use Phoenix.Component, global_prefixes: ~w(pop-)

      def handle_event("llv_server_message", %{"type" => type} = params, socket) do
        handle_server_event(type, params, socket)
      end

      def update(attrs, socket) do
        atomized = Map.new(attrs, fn
          {k, v} when is_binary(k) -> {String.to_atom(k), v}
          {k, v} -> {k, v}
        end)

        {:ok, Phoenix.Component.assign(socket, atomized)}
      end

      def handle_server_event(_, _, socket) do
        {:noreply, socket}
      end

      defoverridable update: 2, handle_server_event: 3
    end
  end

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

  @type unsigned_params :: map

  @callback render(assigns :: Socket.assigns()) :: map()

  @callback mount(
              params :: unsigned_params() | :not_mounted_at_router,
              session :: map,
              socket :: Socket.t()
            ) ::
              {:ok, Socket.t()} | {:ok, Socket.t(), keyword()}

  @callback handle_event(event :: binary, unsigned_params(), socket :: Socket.t()) ::
              {:noreply, Socket.t()} | {:reply, map, Socket.t()}

  @callback update(attrs :: map(), socket :: Socket.t()) :: {:ok, Socket.t()}
end
