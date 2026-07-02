defmodule LocalLiveView.Channel do
  use Phoenix.Channel

  def get_mirror_assigns(llv_id) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, llv_id) do
      [{pid, _}] -> GenServer.call(pid, :get_mirror_assigns)
      [] -> %{}
    end
  end

  def set_mirror_assigns(llv_id, assigns) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, llv_id) do
      [{pid, _}] -> GenServer.call(pid, {:set_mirror_assigns, assigns})
      [] -> {:error, :not_found}
    end
  end

  def join("llv:" <> llv_id, %{"view" => view_string, "token" => token}, socket) do
    case LocalLiveView.MirrorToken.verify(socket.endpoint, token, max_age: :infinity) do
      {:ok, %{id: ^llv_id, view: ^view_string}} ->
        case Registry.register(LocalLiveView.ChannelRegistry, llv_id, view_string) do
          {:ok, _} ->
            mirror_module = find_mirror_module(view_string)

            {:ok,
             assign(socket, llv_id: llv_id, mirror_assigns: %{}, mirror_module: mirror_module)}

          {:error, {:already_registered, _pid}} ->
            {:error, %{reason: "already_joined"}}
        end

      {:error, _} ->
        {:error, %{reason: "unauthorized"}}
    end
  end

  def join(_topic, _params, _socket) do
    {:error, %{reason: "unauthorized"}}
  end

  def handle_call(:get_mirror_assigns, _from, socket) do
    {:reply, socket.assigns.mirror_assigns, socket}
  end

  def handle_call({:set_mirror_assigns, assigns}, _from, socket) do
    push(socket, "set_assigns", assigns)
    {:reply, :ok, socket}
  end

  def handle_in("sync", local_assigns, socket) do
    session = %{llv_id: socket.assigns.llv_id}

    new_mirror_assigns =
      merge_assigns(
        socket.assigns.mirror_module,
        local_assigns,
        socket.assigns.mirror_assigns,
        session
      )

    {:noreply, assign(socket, mirror_assigns: new_mirror_assigns)}
  end

  defp find_mirror_module(view_string) do
    mirror = String.to_existing_atom("Elixir.Mirror." <> view_string)

    if Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 3),
      do: mirror,
      else: nil
  rescue
    ArgumentError -> nil
  end

  defp merge_assigns(nil, local_assigns, _mirror_assigns, _session), do: local_assigns

  defp merge_assigns(mirror, local_assigns, mirror_assigns, session) do
    {:ok, new_mirror_assigns} = mirror.handle_sync(local_assigns, mirror_assigns, session)
    new_mirror_assigns
  end
end
