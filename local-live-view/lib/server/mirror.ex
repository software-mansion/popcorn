defmodule LocalLiveView.Mirror do
  @moduledoc ~S'''
  Behaviour for server-side mirror modules that receive synced assigns from a LocalLiveView.

  A mirror module is automatically discovered by naming convention: `Mirror.<ViewName>`.
  It receives synced payloads from the local runtime via the `handle_sync/3` callback.

  ```
  defmodule Mirror.MyLocal do
    use LocalLiveView.Mirror

    @impl true
    def handle_sync(local_assigns, _mirror_assigns, %{mirror_id: mirror_id}) do
      Phoenix.PubSub.broadcast(MyApp.PubSub, "llv_mirror:MyLive:#{mirror_id}", {:llv_attrs, local_assigns})
      {:ok, local_assigns}
    end
  end
  ```
  '''

  @doc """
  Acts as a conflict resolution point between the local LiveView and its server-side mirror.
  Receives `local_assigns` (the map of synced assigns from the local runtime),
  `mirror_assigns` (the current state stored in the mirror channel), and
  `session` (a map containing `:llv_id` — the unique id of the LLV instance).
  Must return `{:ok, new_mirror_assigns}`.
  """
  @callback handle_sync(local_assigns :: map(), mirror_assigns :: map(), session :: map()) ::
              {:ok, map()}

  @doc false
  def find_module(view_string) do
    mirror =
      try do
        String.to_existing_atom("Elixir.Mirror." <> view_string)
      rescue
        ArgumentError -> nil
      end

    if mirror != nil and Code.ensure_loaded?(mirror) and
         function_exported?(mirror, :handle_sync, 3) do
      mirror
    else
      nil
    end
  end

  @doc """
  Returns the mirror assigns currently held for `mirror_id`.

  These are the assigns last returned by the view's
  `c:LocalLiveView.Mirror.handle_sync/3`. Returns an empty map when no local
  live view is currently joined under that id, for example before the WASM
  runtime has started.

  The browser is the source of truth here — mirror assigns are the server's view
  of the state that LocalLiveView last synced, kept in the channel process. A
  temporary disconnect does not lose it: the channel rejoins under the same
  `mirror_id`, because the id is derived from the host LiveView's `socket.id`,
  which survives reconnects. The mirror is then brought up to date by the next
  `LocalLiveView.mirror_sync/2`. Reloading the page starts a new `mirror_id`,
  since the LocalLiveView starts from scratch too.

  ```
  def handle_info({:synced, mirror_id}, socket) do
    users = LocalLiveView.Mirror.get_mirror_assigns(mirror_id) |> Map.get("users", [])
    {:noreply, assign(socket, :users, users)}
  end
  ```
  """
  def get_mirror_assigns(mirror_id) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, mirror_id) do
      [{pid, _}] -> GenServer.call(pid, :get_mirror_assigns)
      [] -> %{}
    end
  end

  defmacro __using__(_opts) do
    quote do
      @behaviour LocalLiveView.Mirror
    end
  end
end
