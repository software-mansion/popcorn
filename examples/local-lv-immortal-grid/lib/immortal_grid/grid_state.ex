defmodule ImmortalGrid.GridState do
  @moduledoc """
  GenServer holding the grid state for The Immortal Grid.

  The grid is a map: %{ {x, y} => cell } where cell is:
    %{nick: string, owner_id: string, timestamp: integer, claimed_offline: boolean}

  On resync (restore_cells), cells are merged using last-write-wins by timestamp.
  Each recovered cell is broadcast individually with a stagger for visual drama.
  """

  use GenServer

  @pubsub ImmortalGrid.PubSub
  @topic "grid:updates"
  @clients_topic "grid:clients"
  @resync_delay_ms 75

  # ── Public API ──────────────────────────────────────────────────────────────

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def get_all do
    GenServer.call(__MODULE__, :get_all)
  end

  def claim_cell(x, y, nick, owner_id, timestamp, claimed_offline \\ false) do
    GenServer.call(__MODULE__, {:claim_cell, x, y, nick, owner_id, timestamp, claimed_offline})
  end

  def release_cell(x, y, owner_id) do
    GenServer.call(__MODULE__, {:release_cell, x, y, owner_id})
  end

  def restore_cells(cell_list) when is_list(cell_list) do
    GenServer.cast(__MODULE__, {:restore_cells, cell_list})
  end

  def clear do
    GenServer.cast(__MODULE__, :clear)
  end

  def track_client(client_id, :connected) do
    Phoenix.PubSub.broadcast(@pubsub, @clients_topic, {:client_connected, client_id})
  end

  def track_client(client_id, :disconnected) do
    Phoenix.PubSub.broadcast(@pubsub, @clients_topic, {:client_disconnected, client_id})
  end

  # ── GenServer callbacks ──────────────────────────────────────────────────────

  @impl true
  def init(_) do
    {:ok, %{}}
  end

  @impl true
  def handle_call(:get_all, _from, cells) do
    {:reply, cells, cells}
  end

  @impl true
  def handle_call({:claim_cell, x, y, nick, owner_id, timestamp, claimed_offline}, _from, cells) do
    cell = %{
      nick: nick,
      owner_id: owner_id,
      timestamp: timestamp,
      claimed_offline: claimed_offline
    }

    new_cells = Map.put(cells, {x, y}, cell)

    Phoenix.PubSub.broadcast(@pubsub, @topic, {
      :cell_claimed,
      %{
        x: x,
        y: y,
        nick: nick,
        owner_id: owner_id,
        timestamp: timestamp,
        claimed_offline: claimed_offline
      }
    })

    {:reply, :ok, new_cells}
  end

  @impl true
  def handle_call({:release_cell, x, y, owner_id}, _from, cells) do
    case Map.get(cells, {x, y}) do
      %{owner_id: ^owner_id} ->
        new_cells = Map.delete(cells, {x, y})
        Phoenix.PubSub.broadcast(@pubsub, @topic, {:cell_released, %{x: x, y: y}})
        {:reply, :ok, new_cells}

      _ ->
        {:reply, {:error, :not_owner}, cells}
    end
  end

  @impl true
  def handle_cast({:restore_cells, cell_list}, cells) do
    # Merge cells using last-write-wins by timestamp
    {merged_cells, broadcast_queue} =
      Enum.reduce(cell_list, {cells, []}, fn raw_cell, {acc, queue} ->
        x = raw_cell["x"]
        y = raw_cell["y"]
        nick = raw_cell["nick"] || "?"
        owner_id = raw_cell["owner_id"] || ""
        timestamp = raw_cell["timestamp"] || 0
        claimed_offline = raw_cell["claimed_offline"] || false

        existing = Map.get(acc, {x, y})

        if is_nil(existing) or timestamp > existing.timestamp do
          cell = %{
            nick: nick,
            owner_id: owner_id,
            timestamp: timestamp,
            claimed_offline: claimed_offline
          }

          {Map.put(acc, {x, y}, cell), [{x, y, cell} | queue]}
        else
          {acc, queue}
        end
      end)

    # Broadcast each recovered cell with a stagger for visual drama
    broadcast_queue
    |> Enum.reverse()
    |> Enum.with_index()
    |> Enum.each(fn {{x, y, cell}, idx} ->
      delay = idx * @resync_delay_ms
      Process.send_after(self(), {:broadcast_restored_cell, x, y, cell}, delay)
    end)

    {:noreply, merged_cells}
  end

  @impl true
  def handle_cast(:clear, _cells) do
    Phoenix.PubSub.broadcast(@pubsub, @topic, {:grid_reset, %{}})
    {:noreply, %{}}
  end

  @impl true
  def handle_info({:broadcast_restored_cell, x, y, cell}, cells) do
    Phoenix.PubSub.broadcast(@pubsub, @topic, {
      :cell_claimed,
      %{
        x: x,
        y: y,
        nick: cell.nick,
        owner_id: cell.owner_id,
        timestamp: cell.timestamp,
        claimed_offline: cell.claimed_offline
      }
    })

    {:noreply, cells}
  end

  # ── Helpers ──────────────────────────────────────────────────────────────────

  @doc """
  Serializes the cells map to a list of maps for JSON/LLV transport.
  """
  def serialize(cells) do
    Enum.map(cells, fn {{x, y}, cell} ->
      %{
        "x" => x,
        "y" => y,
        "nick" => cell.nick,
        "owner_id" => cell.owner_id,
        "timestamp" => cell.timestamp,
        "claimed_offline" => cell.claimed_offline
      }
    end)
  end
end
