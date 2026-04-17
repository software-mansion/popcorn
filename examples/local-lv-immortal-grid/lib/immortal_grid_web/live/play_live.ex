defmodule ImmortalGridWeb.PlayLive do
  @moduledoc """
  Server-side LiveView wrapper for the /play route.

  This LiveView:
  1. Mounts the Local LiveView (WASM) that handles the interactive grid
  2. Subscribes to PubSub and forwards grid updates to the local LV
  3. Receives events from the local LV (claim_cell, release_cell, restore_cells)
     and persists them to GridState
  4. Handles reconnection: asks the local LV to push its local state on reconnect
  """
  use ImmortalGridWeb, :live_view

  alias ImmortalGrid.GridState

  @pubsub ImmortalGrid.PubSub
  @local_view "PlayLocal"

  @impl true
  def mount(_params, _session, socket) do
    client_id =
      :crypto.strong_rand_bytes(8) |> Base.url_encode64(padding: false)

    if connected?(socket) do
      Phoenix.PubSub.subscribe(@pubsub, "grid:updates")
      GridState.track_client(client_id, :connected)
      # Ask local LV to initialize itself once it mounts
      send(self(), :push_initial_state)
    end

    {:ok,
     socket
     |> assign(:client_id, client_id)
     |> assign(:page_title, "Play — The Immortal Grid")}
  end

  @impl true
  def terminate(_reason, socket) do
    if socket.assigns[:client_id] do
      GridState.track_client(socket.assigns.client_id, :disconnected)
    end

    :ok
  end

  # ── PubSub: forward grid updates to the local LV ────────────────────────────

  @impl true
  def handle_info(:push_initial_state, socket) do
    cells = GridState.get_all()
    serialized = GridState.serialize(cells)

    socket =
      push_to_local(socket, @local_view, %{
        "type" => "initialize",
        "cells" => serialized,
        "owner_id" => socket.assigns.client_id
      })

    {:noreply, socket}
  end

  def handle_info({:cell_claimed, cell}, socket) do
    socket =
      push_to_local(socket, @local_view, %{
        "type" => "cell_update",
        "x" => cell.x,
        "y" => cell.y,
        "nick" => cell.nick,
        "owner_id" => cell.owner_id,
        "timestamp" => cell.timestamp,
        "claimed_offline" => cell.claimed_offline
      })

    {:noreply, socket}
  end

  def handle_info({:cell_released, %{x: x, y: y}}, socket) do
    socket =
      push_to_local(socket, @local_view, %{"type" => "cell_released", "x" => x, "y" => y})

    {:noreply, socket}
  end

  def handle_info({:grid_reset, _}, socket) do
    socket = push_to_local(socket, @local_view, %{"type" => "grid_reset"})
    {:noreply, socket}
  end

  def handle_info({:client_connected, _}, socket), do: {:noreply, socket}
  def handle_info({:client_disconnected, _}, socket), do: {:noreply, socket}

  # ── Render ───────────────────────────────────────────────────────────────────

  @impl true
  def render(assigns) do
    ~H"""
    <div id="play-wrapper" class="min-h-screen" style="background: #fffdf5;">
      <.local_live_view view="PlayLocal" id="PlayLocal" />
    </div>
    """
  end
end
