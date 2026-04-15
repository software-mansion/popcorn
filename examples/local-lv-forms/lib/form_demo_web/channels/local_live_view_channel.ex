defmodule LocalLiveView.Channel do
  use Phoenix.Channel

  def get_attrs(llv_id) do
    case Registry.lookup(LocalLiveView.ChannelRegistry, llv_id) do
      [{pid, _}] -> GenServer.call(pid, :get_attrs)
      [] -> %{}
    end
  end

  def join("llv:" <> llv_id, %{"view" => view_string}, socket) do
    Registry.register(LocalLiveView.ChannelRegistry, llv_id, nil)
    mirror_module = find_mirror_module(view_string)
    {:ok, assign(socket, llv_id: llv_id, attrs: %{}, mirror_module: mirror_module)}
  end

  def handle_call(:get_attrs, _from, socket) do
    {:reply, socket.assigns.attrs, socket}
  end

  def handle_in("sync", payload, socket) do
    new_attrs = merge_attrs(socket.assigns.mirror_module, payload, socket.assigns.attrs)
    {:noreply, assign(socket, attrs: new_attrs)}
  end

  defp find_mirror_module(view_string) do
    mirror = String.to_atom("Elixir.Mirror." <> view_string)

    if Code.ensure_loaded?(mirror) and function_exported?(mirror, :handle_sync, 2),
      do: mirror,
      else: nil
  end

  defp merge_attrs(nil, payload, _attrs), do: payload

  defp merge_attrs(mirror, payload, attrs) do
    {:ok, new_attrs} = mirror.handle_sync(payload, attrs)
    new_attrs
  end
end
