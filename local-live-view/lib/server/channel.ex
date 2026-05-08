defmodule LocalLiveView.Channel do
  use Phoenix.Channel

  def get_mirror_assigns(llv_id) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, llv_id) do
      [{pid, _}] -> GenServer.call(pid, :get_mirror_assigns)
      [] -> %{}
    end
  end

  def get_all_mirror_assigns(llv_id) do
    Registry.select(LocalLiveView.ChannelRegistry, [
      {{:"$1", :"$2", :"$3"}, [{:==, :"$3", llv_id}], [:"$2"]}
    ])
    |> Enum.map(&GenServer.call(&1, :get_mirror_assigns))
  end

  def set_mirror_assigns(llv_id, assigns) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, llv_id) do
      [{pid, _}] -> GenServer.call(pid, {:set_mirror_assigns, assigns})
      [] -> {:error, :not_found}
    end
  end

  def join("llv:" <> llv_id, %{"view" => view_string}, socket) do
    Registry.register(LocalLiveView.ChannelRegistry, llv_id, view_string)
    mirror_module = find_mirror_module(view_string)
    {:ok, assign(socket, llv_id: llv_id, mirror_assigns: %{}, mirror_module: mirror_module)}
  end

  def handle_call(:get_mirror_assigns, _from, socket) do
    {:reply, socket.assigns.mirror_assigns, socket}
  end

  def handle_call({:set_mirror_assigns, assigns}, _from, socket) do
    push(socket, "set_assigns", assigns)
    {:reply, :ok, socket}
  end

  def handle_in("sync", local_assigns, socket) do
    enriched = Map.put(local_assigns, "_llv_id", socket.assigns.llv_id)

    new_mirror_assigns =
      merge_assigns(socket.assigns.mirror_module, enriched, socket.assigns.mirror_assigns)

    {:noreply, assign(socket, mirror_assigns: new_mirror_assigns)}
  end

  defp find_mirror_module(view_string) do
    mirror = Module.concat(Mirror, view_string)

    if Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 2),
      do: mirror,
      else: nil
  end

  defp merge_assigns(nil, local_assigns, _mirror_assigns), do: local_assigns

  defp merge_assigns(mirror, local_assigns, mirror_assigns) do
    {:ok, new_mirror_assigns} = mirror.handle_sync(local_assigns, mirror_assigns)
    new_mirror_assigns
  end
end
