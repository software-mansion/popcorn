defmodule LocalLiveView.Channel do
  @moduledoc false
  # The channel backing LocalLiveView mirror synchronization.

  # Each LocalLiveView that syncs its assigns joins this channel on its own
  # topic, identified by the `mirror_id` obtained from
  # `LocalLiveView.Component.mirror_id/2`. The channel keeps the mirror assigns
  # returned by `c:LocalLiveView.Mirror.handle_sync/3` and makes them readable
  # from the server with `get_mirror_assigns/1`.

  # The channel is mounted by `LocalLiveView.Socket`, so there is nothing to wire
  # up by hand — `mix llv.install` adds the socket to your endpoint and the
  # `LocalLiveView.ChannelRegistry` to your supervision tree.

  use Phoenix.Channel

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
