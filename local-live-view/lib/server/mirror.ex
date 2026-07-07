defmodule LocalLiveView.Mirror do
  @moduledoc ~S'''
  Behaviour for server-side mirror modules that receive synced assigns from a LocalLiveView.

  A mirror module is automatically discovered by naming convention: `Mirror.<ViewName>`.
  It receives synced payloads from the local runtime via the `handle_sync/3` callback.

  ```
  defmodule Mirror.MyLive do
    use LocalLiveView.Mirror

    @impl true
    def handle_sync(local_assigns, _mirror_assigns, %{llv_id: llv_id}) do
      Phoenix.PubSub.broadcast(MyApp.PubSub, "llv_mirror:MyLive:#{llv_id}", {:llv_attrs, local_assigns})
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

  defmacro __using__(_opts) do
    quote do
      @behaviour LocalLiveView.Mirror
    end
  end
end
