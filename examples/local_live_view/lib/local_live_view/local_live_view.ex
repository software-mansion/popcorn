defmodule LocalLiveView do
  @moduledoc ~S'''
  LocalLiveView is a module that implements functionality of Phoenix.LiveView inside the browser in  
  Popcorn runtime.
  LocalLiveView should be used exactly like its Phoenix equivalent:
  
      defmodule MyAppWeb.DemoLive do
        use Phoenix.LiveView

        def render(assigns) do
          ~H"""
          Hello world!
          """
        end
      end
  
  The LocalLiveView can be added to a page with:
  ```
  <div data-pop-view="DemoLive"></div>
  ```
  
  During application runtime a LocalLiveView.Dispatcher will create a process that will handle a LocalLiveView state,
  by storing and modifying its assigns.
  '''
  
  alias Phoenix.LiveView.Session
  alias Phoenix.LiveView.Diff
  import Popcorn.Wasm
  alias Popcorn.Wasm
  alias Phoenix.LiveView.Static
  alias LocalLiveView.Message

  defmacro __using__(opts) do
    quote bind_quoted: [opts: []] do
      import LocalLiveView
      @before_compile Phoenix.LiveView.Renderer
      @phoenix_live_opts []
      Module.register_attribute(__MODULE__, :phoenix_live_mount, accumulate: true)
      @before_compile LocalLiveView

      alias LocalLiveView.Message
      use Phoenix.Component, Keyword.take(opts, [:global_prefixes])
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

  def render(assigns) do
    :ok
  end
end
