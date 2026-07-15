defmodule LocalLvKanbanWeb.BoardsLive do
  @moduledoc """
  Index of kanban boards: list existing boards and create new ones. Plain server
  LiveView (no local live view) — boards are persisted in the DB.
  """
  use LocalLvKanbanWeb, :live_view

  alias LocalLvKanban.Boards

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, boards: Boards.list_boards())}
  end

  @impl true
  def handle_event("create", params, socket) do
    case Boards.create_board(params) do
      {:ok, board} -> {:noreply, push_navigate(socket, to: ~p"/boards/#{board.id}")}
      {:error, _} -> {:noreply, socket}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div style="min-height:100vh;background:#0b1220;color:#e5e7eb;font-family:sans-serif;padding:2em">
      <.flash kind={:error} flash={@flash} />
      <div style="max-width:640px;margin:0 auto">
        <h1 style="font-size:1.8em;font-weight:700;color:#f9fafb;margin:0 0 1em">Kanban boards</h1>

        <form
          phx-submit="create"
          style="display:flex;gap:0.5em;margin-bottom:1.5em"
          autocomplete="off"
        >
          <input
            type="text"
            name="name"
            required
            placeholder="New board name..."
            style="flex:1;background:#1f2937;color:#f3f4f6;border:1px solid #374151;border-radius:6px;padding:0.6em 0.7em;font-size:1em;outline:none"
          />
          <button
            type="submit"
            style="background:#2563eb;color:#fff;border:none;border-radius:6px;padding:0.6em 1.1em;font-size:1em;cursor:pointer"
          >
            Create
          </button>
        </form>

        <div :if={@boards == []} style="color:#6b7280;font-style:italic">
          No boards yet — create one above.
        </div>
        <ul style="list-style:none;padding:0;margin:0;display:flex;flex-direction:column;gap:0.5em">
          <li :for={board <- @boards}>
            <.link
              navigate={~p"/boards/#{board.id}"}
              style="display:block;background:#1f2937;border:1px solid #374151;border-radius:8px;padding:0.8em 1em;color:#f3f4f6;text-decoration:none;font-size:1.05em"
            >
              {board.name}
            </.link>
          </li>
        </ul>
      </div>
    </div>
    """
  end
end
