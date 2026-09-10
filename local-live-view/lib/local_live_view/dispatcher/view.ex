defmodule LocalLiveView.Dispatcher.View do
  @moduledoc false
  # Utilities for managing views in the dispatcher

  @enforce_keys [:epoch]

  defstruct @enforce_keys ++
              [
                channel_pid: nil,
                monitor_ref: nil,
                pending: []
              ]

  def render_container(id, session) do
    parent = %Phoenix.LiveView.Socket{
      endpoint: LocalLiveView.Endpoint,
      transport_pid: self(),
      host_uri: :not_mounted_at_router,
      assigns: %{__assigns__: %{}}
    }

    {:safe, iodata} =
      Phoenix.Component.live_render(parent, LocalLiveView.Proxy,
        id: id,
        # We create a sticky view, because it's the only kind
        # of view that's completely controlled by its channel
        # and thus easiest to isolate from the host LV and can
        # run with no host LV at all.
        sticky: true,
        container: {:div, data: [pop_root: true]},
        session: session
      )

    IO.iodata_to_binary(iodata)
  end

  def register_channel(%__MODULE__{} = view, pid, monitor_ref) do
    for msg <- Enum.reverse(view.pending), do: send(pid, msg)
    %{view | channel_pid: pid, monitor_ref: monitor_ref, pending: []}
  end

  def dispatch(view, msg) do
    if pid = view.channel_pid, do: send(pid, msg)
    :ok
  end

  @doc """
  Sends a message to the view, queueing it until the view's
  channel registers.
  """
  def dispatch_queue(%__MODULE__{channel_pid: pid} = view, msg) when pid != nil do
    send(pid, msg)
    view
  end

  def dispatch_queue(%__MODULE__{} = view, msg) do
    %{view | pending: [msg | view.pending]}
  end

  def channel_down(%__MODULE__{} = view) do
    %{view | channel_pid: nil, monitor_ref: nil}
  end
end
