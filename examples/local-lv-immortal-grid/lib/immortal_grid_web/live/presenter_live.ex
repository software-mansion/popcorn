defmodule ImmortalGridWeb.PresenterLive do
  @moduledoc """
  Classic (server-rendered) LiveView for the conference presenter.

  Shows the full grid in real-time via PubSub. When the server dies,
  Phoenix adds `phx-disconnected` class to the HTML element, which
  triggers CSS rules that show an overlay and fade the grid.

  When server restarts and reconnects, auto-syncs via existing Phoenix
  LiveView reconnection mechanism.
  """
  use ImmortalGridWeb, :live_view

  alias ImmortalGrid.GridState

  @grid_cols 20
  @grid_rows 10
  @pubsub ImmortalGrid.PubSub

  @impl true
  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(@pubsub, "grid:updates")
      Phoenix.PubSub.subscribe(@pubsub, "grid:clients")
    end

    cells = GridState.get_all()

    {:ok,
     socket
     |> assign(:cells, cells)
     |> assign(:client_count, 0)
     |> assign(:syncing, false)
     |> assign(:recovered_count, 0)
     |> assign(:offline_count, 0)
     |> assign(:grid_cols, @grid_cols)
     |> assign(:grid_rows, @grid_rows)
     |> assign(:page_title, "Presenter — The Immortal Grid")}
  end

  @impl true
  def handle_info({:cell_claimed, cell}, socket) do
    cells =
      Map.put(socket.assigns.cells, {cell.x, cell.y}, %{
        nick: cell.nick,
        owner_id: cell.owner_id,
        timestamp: cell.timestamp,
        claimed_offline: cell.claimed_offline
      })

    offline_bump = if cell.claimed_offline, do: 1, else: 0

    {:noreply,
     socket
     |> assign(:cells, cells)
     |> update(:offline_count, &(&1 + offline_bump))
     |> update(:recovered_count, fn n ->
       if cell.claimed_offline, do: n + 1, else: n
     end)}
  end

  def handle_info({:cell_released, %{x: x, y: y}}, socket) do
    cells = Map.delete(socket.assigns.cells, {x, y})
    {:noreply, assign(socket, :cells, cells)}
  end

  def handle_info({:grid_reset, _}, socket) do
    {:noreply,
     socket
     |> assign(:cells, %{})
     |> assign(:recovered_count, 0)
     |> assign(:offline_count, 0)}
  end

  def handle_info({:client_connected, _id}, socket) do
    {:noreply, update(socket, :client_count, &(&1 + 1))}
  end

  def handle_info({:client_disconnected, _id}, socket) do
    {:noreply, update(socket, :client_count, &max(&1 - 1, 0))}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div
      id="presenter-root"
      class="min-h-screen flex flex-col select-none"
      style="background: #fffdf5; font-family: 'Inter', sans-serif;"
    >
      <%!-- ── Header ── --%>
      <header
        class="px-6 py-4 flex items-center justify-between border-b"
        style="border-color: #efecea;"
      >
        <h1
          class="text-4xl font-bold tracking-tight"
          style="font-family: 'Handjet Variable', system-ui; color: #58330c; letter-spacing: 0.05em;"
        >
          The Immortal Grid
        </h1>
        <div class="flex items-center gap-6">
          <%!-- Status dot (CSS-controlled on disconnect) --%>
          <div class="flex items-center gap-2">
            <div
              id="status-dot"
              class="w-2.5 h-2.5 rounded-full transition-colors"
              style="background: #22c55e;"
            >
            </div>
            <span
              id="status-text"
              class="text-xs font-medium"
              style="color: #5f4122;"
            >
              connected
            </span>
          </div>
          <%!-- Player count --%>
          <div class="text-sm" style="color: #5f4122;">
            <span class="font-bold text-base" style="color: #58330c;">{@client_count}</span>
            {if @client_count == 1, do: "player", else: "players"}
          </div>
          <%!-- Cell count --%>
          <div class="text-sm" style="color: #5f4122;">
            <span class="font-bold text-base" style="color: #58330c;">{map_size(@cells)}</span>
            / {@grid_cols * @grid_rows} cells
          </div>
          <%!-- Offline cells badge --%>
          <div
            :if={@offline_count > 0}
            class="px-2 py-1 rounded text-xs font-bold"
            style="background: #fffbf0; color: #b26f00; border: 1px solid #ffe2a8;"
          >
            +{@offline_count} offline
          </div>
        </div>
      </header>

      <%!-- ── Offline overlay (visible when phx-disconnected) ── --%>
      <div
        id="offline-overlay"
        class="fixed inset-0 items-center justify-center z-50 pointer-events-none"
        style="display: none; background: rgba(48, 27, 5, 0.7);"
      >
        <div
          class="rounded-2xl p-10 text-center shadow-2xl"
          style="background: #fff5f5; border: 2px solid #fbbcbc; max-width: 400px;"
        >
          <div
            class="text-7xl mb-4"
            style="font-family: 'Handjet Variable', system-ui; color: #cf3f3f;"
          >
            💀
          </div>
          <h2
            class="text-4xl font-bold mb-3"
            style="font-family: 'Handjet Variable', system-ui; color: #b52e2e; letter-spacing: 0.1em;"
          >
            SERVER OFFLINE
          </h2>
          <p class="text-base" style="color: #cf3f3f; font-family: 'Inter', sans-serif;">
            Players continue locally.<br />Grid will sync on resurrection.
          </p>
        </div>
      </div>

      <%!-- ── Grid ── --%>
      <div class="flex-1 flex items-center justify-center p-6" id="grid-wrap">
        <div
          id="grid-container"
          style={"display: grid; grid-template-columns: repeat(#{@grid_cols}, 1fr); gap: 3px;"}
        >
          <%= for y <- 0..(@grid_rows - 1), x <- 0..(@grid_cols - 1) do %>
            <% cell = Map.get(@cells, {x, y}) %>
            <div
              id={"cell-#{x}-#{y}"}
              class={["grid-cell", cell_css_class(cell)]}
              style="width: 42px; height: 42px; display: flex; align-items: center; justify-content: center; border-radius: 3px; overflow: hidden; transition: background 0.3s ease;"
              title={if cell, do: cell.nick, else: nil}
            >
              <span
                :if={cell}
                style="font-family: 'Handjet Variable', system-ui; font-size: 11px; color: white; text-align: center; line-height: 1.1; word-break: break-all; padding: 2px; width: 100%;"
              >
                {cell.nick}
              </span>
            </div>
          <% end %>
        </div>
      </div>

      <%!-- ── Resync banner ── --%>
      <div
        :if={@recovered_count > 0}
        class="px-6 py-2 text-sm font-medium"
        style="background: #fffbf0; color: #b26f00; border-top: 1px solid #ffe2a8;"
      >
        ✓ Resync complete — recovered
        <strong>{@recovered_count}</strong>
        cells ({@offline_count} claimed offline)
      </div>

      <%!-- ── Legend footer ── --%>
      <footer
        class="px-6 py-3 flex items-center gap-6 border-t text-xs"
        style="border-color: #efecea; color: #696057;"
      >
        <div class="flex items-center gap-2">
          <div
            class="w-4 h-4 rounded-sm"
            style="background: #f5f2f0; border: 1px solid #efecea;"
          >
          </div>
          <span>Empty</span>
        </div>
        <div class="flex items-center gap-2">
          <div class="w-4 h-4 rounded-sm" style="background: #ef7c00;"></div>
          <span>Claimed</span>
        </div>
        <div class="flex items-center gap-2">
          <div class="w-4 h-4 rounded-sm" style="background: #d48f00;"></div>
          <span>Claimed offline</span>
        </div>
        <div class="ml-auto" style="color: #696057;">
          Players: <strong style="color: #58330c;">localhost:4000/play</strong>
        </div>
      </footer>
    </div>
    """
  end

  # ── Helpers ──────────────────────────────────────────────────────────────────

  defp cell_css_class(nil), do: "cell-empty"
  defp cell_css_class(%{claimed_offline: true}), do: "cell-offline"
  defp cell_css_class(_), do: "cell-claimed"
end
