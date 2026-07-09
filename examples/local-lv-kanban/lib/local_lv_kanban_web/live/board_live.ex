defmodule LocalLvKanbanWeb.BoardLive do
  @moduledoc """
  Server LiveView that hosts the `Local.Kanban` local live view for one board.

  Server-authoritative collaborative flow:

    * The browser-side view applies edits optimistically and notifies this view
      (`targets([@default, @server])` for adds/removes, `push_server_event` for drag-moves).
    * `handle_event` writes the edit to the DB. On success it broadcasts
      `:board_changed` to every `BoardLive` viewing this board (including itself);
      each re-reads the board and re-assigns it, so all clients converge.
    * On failure it re-pushes the (unchanged) authoritative board to the origin,
      rolling back the optimistic change. The `rev` counter forces that push even
      though the board value is unchanged.
  """
  use LocalLvKanbanWeb, :live_view

  alias LocalLvKanban.Boards

  @edits ~w(add_column add_task move_task remove_column remove_task)

  @impl true
  def mount(%{"id" => id}, _session, socket) do
    case Boards.get_board(id) do
      nil ->
        # Stale/removed board: pop an error and send them back to the list.
        {:ok,
         socket
         |> put_flash(:error, "That board doesn't exist anymore.")
         |> push_navigate(to: ~p"/")}

      board ->
        if connected?(socket), do: Phoenix.PubSub.subscribe(LocalLvKanban.PubSub, topic(id))

        {:ok,
         assign(socket,
           board_id: id,
           board_name: board.name,
           board: Boards.board_to_data(board),
           rev: 0
         )}
    end
  end

  @impl true
  def handle_event(event, params, socket) when event in @edits do
    case apply_edit(socket.assigns.board_id, event, params) do
      :ok ->
        Phoenix.PubSub.broadcast(
          LocalLvKanban.PubSub,
          topic(socket.assigns.board_id),
          :board_changed
        )

        {:noreply, socket}

      :error ->
        # Roll back the optimistic change on this client by re-pushing the
        # authoritative (unchanged) board.
        {:noreply, push_board(socket)}
    end
  end

  @impl true
  def handle_info(:board_changed, socket) do
    {:noreply, push_board(socket)}
  end

  # Re-read the board from the DB and re-assign it. Always bump `rev` so the
  # browser-side view re-renders even when the board value is unchanged (failure
  # rollback) — `rev` is the client's rebuild trigger.
  defp push_board(socket) do
    board = Boards.board_to_data(Boards.get_board!(socket.assigns.board_id))
    assign(socket, board: board, rev: socket.assigns.rev + 1)
  end

  defp topic(board_id), do: "board:#{board_id}"

  defp apply_edit(board_id, "add_column", params), do: Boards.add_column(board_id, params)
  defp apply_edit(board_id, "add_task", params), do: Boards.add_task(board_id, params)
  defp apply_edit(board_id, "move_task", params), do: Boards.move_task(board_id, params)
  defp apply_edit(board_id, "remove_column", params), do: Boards.remove_column(board_id, params)
  defp apply_edit(board_id, "remove_task", params), do: Boards.remove_task(board_id, params)

  @impl true
  def render(assigns) do
    ~H"""
    <div style="min-height:100vh;background:#0b1220;padding:1em;font-family:sans-serif">
      <.link
        navigate={~p"/"}
        style="color:#93c5fd;text-decoration:none;font-size:0.9em;display:inline-block;margin-bottom:0.5em"
      >
        ← All boards
      </.link>
      <.local_live_view view="Local.Kanban" name={@board_name} board={@board} rev={@rev} />
    </div>
    """
  end
end
