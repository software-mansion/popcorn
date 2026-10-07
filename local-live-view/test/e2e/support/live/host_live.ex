defmodule LocalLiveView.E2E.HostLive do
  # Hosts the Hosted local view, at /hosted (:one) and /hosted2 (:two).
  use LocalLiveView.E2E.Web, :live_view

  @impl true
  def mount(params, _session, socket) do
    room = params["room"] || "default"
    if connected?(socket), do: Phoenix.PubSub.subscribe(LocalLiveView.E2E.PubSub, "room:" <> room)
    {:ok, assign(socket, room: room, count: 0, rev: 0, tab: "none")}
  end

  @impl true
  def handle_params(params, _uri, socket) do
    {:noreply, assign(socket, :tab, params["tab"] || "none")}
  end

  # Confirms the local view's optimistic increment, for every client in the room.
  @impl true
  def handle_event("inc", _params, socket) do
    count = socket.assigns.count + 1

    Phoenix.PubSub.broadcast_from(
      LocalLiveView.E2E.PubSub,
      self(),
      topic(socket),
      {:count, count}
    )

    {:noreply, assign(socket, :count, count)}
  end

  # Rejects the local view's edit: the bumped `rev` makes the host send its
  # unchanged assigns again, which roll the edit back.
  def handle_event("reject", _params, socket) do
    {:noreply, update(socket, :rev, &(&1 + 1))}
  end

  def handle_event("host_inc", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  @impl true
  def handle_info({:count, count}, socket) do
    {:noreply, assign(socket, :count, count)}
  end

  defp topic(socket), do: "room:" <> socket.assigns.room

  defp path(:one), do: "/hosted"
  defp path(:two), do: "/hosted2"

  defp other_page(:one), do: "/hosted2"
  defp other_page(:two), do: "/hosted"

  @impl true
  def render(assigns) do
    ~H"""
    <p id="host-page">page: {@live_action}</p>
    <p id="host-tab">host tab: {@tab}</p>
    <p id="host-count">host count: {@count}</p>
    <button id="host-inc" phx-click="host_inc">host inc</button>
    <.link id="host-link-other" navigate="/other">other page</.link>
    <.link id="host-link-navigate" navigate={other_page(@live_action)}>other hosted page</.link>
    <.link id="host-link-slow" navigate="/slow_host">slow page</.link>
    <.local_live_view
      view="Hosted"
      count={@count}
      rev={@rev}
      tab={@tab}
      path={path(@live_action)}
      nav_to={other_page(@live_action)}
    />
    <.local_live_view :if={@live_action == :one} view="Echo" id="only-on-one" label="only on one" />
    """
  end
end
