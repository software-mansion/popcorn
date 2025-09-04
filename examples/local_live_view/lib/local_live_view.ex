defmodule LocalLiveView do
  use GenServer
  @process_name :main
  alias Phoenix.LiveView.Session
  alias Phoenix.LiveView.Diff
  
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
    params = %{
      "session" => %Session{view: ThermostatLive}
    }
    IO.inspect("LLV start1")
#    with {:ok, pid} <- DynamicSupervisor.start_child(LocalLiveView.Server.Supervisor, LocalLiveView.Server.child_spec([])) do
    ref = make_ref()
    with {:ok, pid} <- LocalLiveView.Server.start_link([]) do
      IO.inspect("LLV start2")
      send(pid, {LocalLiveView.Server, params, {self(), ref}, %Phoenix.Socket{}}) |> IO.inspect(label: "ULALA")
      receive do
        {^ref, {:ok, reply}} ->
          IO.inspect(reply, label: "RECEIVED")
          rendered = 
            reply
            |> Diff.to_iodata()
            |> iodata_to_binary()
          Popcorn.Wasm.run_js("""
          ({ args }) => {
            document.body.innerHTML = args.rendered;
          }
          """, %{rendered: rendered})
          {:ok, reply, pid}

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

    :ignore
  end
  
  @impl true
  def handle_info(any, state) do
    IO.inspect(any, label: "HANDLE_INFO:")
    {:no_reply, state}
  end

  def iodata_to_binary(list_of_binaries) when is_list(list_of_binaries) do
    Enum.reduce(list_of_binaries, "", fn binary, acc ->
      acc <> to_string(binary) 
    end)
  end
  
  def render(assigns) do
    :ok
  end
end
