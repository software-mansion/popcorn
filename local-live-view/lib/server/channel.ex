defmodule LocalLiveView.Channel do
  @moduledoc """
  The channel backing LocalLiveView mirror synchronization.

  Each LocalLiveView that syncs its assigns joins this channel on its own
  topic, identified by the `mirror_id` obtained from
  `LocalLiveView.Component.mirror_id/2`. The channel keeps the mirror assigns
  returned by `c:LocalLiveView.Mirror.handle_sync/3` and makes them readable
  from the server with `get_mirror_assigns/1`.

  The channel is mounted by `LocalLiveView.Socket`, so there is nothing to wire
  up by hand — `mix llv.install` adds the socket to your endpoint and the
  `LocalLiveView.ChannelRegistry` to your supervision tree.
  """

  use Phoenix.Channel

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
    users = LocalLiveView.Channel.get_mirror_assigns(mirror_id) |> Map.get("users", [])
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

  # Meant as the server-to-browser counterpart of `LocalLiveView.mirror_sync/2`,
  # but not wired end to end: this pushes a "set_assigns" frame on the mirror
  # channel and nothing in the JS bundle subscribes to it, so the frame is
  # dropped. Hidden from the docs until the browser side handles it and feeds
  # the assigns into the view's update/2.
  @doc false
  def set_mirror_assigns(mirror_id, assigns) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, mirror_id) do
      [{pid, _}] -> GenServer.call(pid, {:set_mirror_assigns, assigns})
      [] -> {:error, :not_found}
    end
  end

  @impl true
  def join("llv:" <> mirror_id, %{"view" => view_string, "token" => token}, socket) do
    case LocalLiveView.MirrorToken.verify(socket.endpoint, token, max_age: :infinity) do
      {:ok, %{id: ^mirror_id, view: ^view_string}} ->
        Registry.register(LocalLiveView.ChannelRegistry, mirror_id, view_string)
        mirror_module = LocalLiveView.Mirror.find_module(view_string)

        {:ok,
         assign(socket, mirror_id: mirror_id, mirror_assigns: %{}, mirror_module: mirror_module)}

      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  def join(_topic, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  @impl true
  def handle_call(:get_mirror_assigns, _from, socket) do
    {:reply, socket.assigns.mirror_assigns, socket}
  end

  def handle_call({:set_mirror_assigns, assigns}, _from, socket) do
    push(socket, "set_assigns", assigns)
    {:reply, :ok, socket}
  end

  @impl true
  def handle_in("sync", local_assigns, socket) do
    session = %{mirror_id: socket.assigns.mirror_id}

    new_mirror_assigns =
      merge_assigns(
        socket.assigns.mirror_module,
        local_assigns,
        socket.assigns.mirror_assigns,
        session
      )

    {:noreply, assign(socket, mirror_assigns: new_mirror_assigns)}
  end

  defp merge_assigns(nil, local_assigns, _mirror_assigns, _session), do: local_assigns

  defp merge_assigns(mirror, local_assigns, mirror_assigns, session) do
    {:ok, new_mirror_assigns} = mirror.handle_sync(local_assigns, mirror_assigns, session)
    new_mirror_assigns
  end
end
