defmodule LocalLiveView.Mirror do
  @moduledoc ~S'''
  Behaviour for server-side mirror modules that receive synced assigns from a LocalLiveView.

  A mirror module is automatically discovered by naming convention: `Mirror.<ViewName>`.
  It receives synced payloads from the local runtime via the `handle_sync/2` callback.

  ```
  defmodule Mirror.MyLocal do
    use LocalLiveView.Mirror

    @impl true
    def handle_sync(local_assigns, _mirror_assigns) do
      Phoenix.PubSub.broadcast(MyApp.PubSub, "llv_mirror:MyLocal", {:llv_attrs, local_assigns})
      {:ok, local_assigns}
    end
  end
  ```
  '''

  @doc """
  Acts as a conflict resolution point between the local LiveView and its server-side mirror.
  Receives `local_assigns` (the map of synced assigns from the local runtime) and
  `mirror_assigns` (the current state stored in the mirror channel). Must return `{:ok, new_mirror_assigns}`.
  """
  @callback handle_sync(local_assigns :: map(), mirror_assigns :: map()) :: {:ok, map()}

  defmacro __using__(_opts) do
    quote do
      @behaviour LocalLiveView.Mirror
    end
  end
end
