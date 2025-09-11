defmodule LocalLiveView do
  use GenServer
  @process_name :main
  alias Phoenix.LiveView.Session
  alias Phoenix.LiveView.Diff
  import Popcorn.Wasm
  alias Popcorn.Wasm
  alias Phoenix.LiveView.Static
  alias LocalLiveView.Message
  
  defmacro __using__(opts) do
    # Expand layout if possible to avoid compile-time dependencies
#    opts =
#      with true <- Keyword.keyword?(opts),
#           {layout, template} <- Keyword.get(opts, :layout) do
#        layout = Macro.expand(layout, %{__CALLER__ | function: {:__live__, 0}})
#        Keyword.replace!(opts, :layout, {layout, template})
#      else
#        _ -> opts
#      end

    quote bind_quoted: [opts: []] do
      import LocalLiveView
#      @behaviour Phoenix.LiveView
      @before_compile Phoenix.LiveView.Renderer
#
      @phoenix_live_opts []
      Module.register_attribute(__MODULE__, :phoenix_live_mount, accumulate: true)
      @before_compile LocalLiveView

      alias LocalLiveView.Message
#
#      # Phoenix.Component must come last so its @before_compile runs last
      use LocalComponent, Keyword.take(opts, [:global_prefixes])
      
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

  @doc """
  Defines metadata for a LiveView.

  This must be returned from the `__live__` callback.

  It accepts:

    * `:container` - an optional tuple for the HTML tag and DOM attributes to
      be used for the LiveView container. For example: `{:li, style: "color: blue;"}`.

    * `:layout` - configures the layout the LiveView will be rendered in.
      This layout can be overridden by on `c:mount/3` or via the `:layout`
      option in `Phoenix.LiveView.Router.live_session/2`

    * `:log` - configures the log level for the LiveView, either `false`
      or a log level

    * `:on_mount` - a list of tuples with module names and argument to be invoked
      as `on_mount` hooks

  """
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

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(_init_arg) do
    Popcorn.Wasm.register(@process_name)
    view = ThermostatLive
    params = %{
      "session" => %Session{view: view}
    }
#    with {:ok, pid} <- DynamicSupervisor.start_child(LocalLiveView.Server.Supervisor, LocalLiveView.Server.child_spec([])) do
    rendered =
      Static.render(view)
      |> then(fn {:ok, {:safe, content}, assigns} -> diff_iodata_to_binary(content) end)
    Popcorn.Wasm.run_js("""
    ({ args }) => {
      document.body.innerHTML = args.rendered;
    }
    """, %{rendered: rendered})
    ref = make_ref()
    with {:ok, pid} <- LocalLiveView.Server.start_link([]) do
      send(pid, {LocalLiveView.Server, params, {self(), ref}, %Phoenix.Socket{}})
      receive do
        {^ref, {:ok, reply}} ->
          reply
          |> Diff.to_iodata()
          |> diff_iodata_to_binary()
          |> LocalLiveView.JS.rerender(view)
          {:ok, %{view => pid}}

        {^ref, {:error, reply}} ->
          {:error, reply}
      end
    end
    
#    rendered = 
#      MyLocalComponent.greet(%{name: "Franek"})
#      |> Phoenix.HTML.Safe.to_iodata()
#      |> iodata_to_binary()
#    Popcorn.Wasm.run_js("""
#    ({ args }) => {
#      document.body.innerHTML = args.rendered;
#    }
#    """, %{rendered: rendered})
#    {:ok, %{view => pid}}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    state = Wasm.handle_message!(raw_msg, &handle_wasm(&1, state))
    {:noreply, state}
  end
  
  def handle_info(any, state) do
    IO.inspect(any, label: "HANDLE_INFO:")
    {:noreply, state}
  end

  defp handle_wasm({:wasm_call, %{"event" => type, "view" => view, "payload" => payload}}, state) do
    view = String.to_existing_atom(view)
    payload = payload |> Map.merge(%{"type" => %{}, "value" => %{}})
    Map.get(state, view)
    |> send(%Message{payload: payload, event: "event"})
    {:resolve, :ok, state}
  end

  def diff_iodata_to_binary(list_of_binaries) when is_list(list_of_binaries) do
    Enum.reduce(list_of_binaries, "", fn
      integer, acc when is_integer(integer) -> acc <> List.to_string([integer])
      list, acc when is_list(list) -> acc <> diff_iodata_to_binary(list)
      binary, acc -> acc <> to_string(binary)
    end)
  end

  def rendered_iodata_to_binary(list_of_binaries) when is_list(list_of_binaries) do
    Enum.reduce(list_of_binaries, "", fn
      integer, acc when is_integer(integer) -> acc <> to_string(integer)
      list, acc when is_list(list) -> acc <> rendered_iodata_to_binary(list)
      binary, acc -> acc <> to_string(binary)
    end)
  end
  
  def render(assigns) do
    :ok
  end
end
