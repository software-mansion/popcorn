defmodule LocalLvKanbanWeb.BoardsLive do
  @moduledoc """
  Landing page: "create new board" / "generate sample board" buttons plus the
  visitor's recent boards. The server keeps no per-visitor state — recents live
  in the browser's localStorage (written by `BoardLive` whenever a board is
  opened) and the `.RecentBoards` hook pushes them up after connect; the ids
  are then re-checked against the DB, so stale entries drop out.
  """
  use LocalLvKanbanWeb, :live_view

  alias LocalLvKanban.Boards

  @impl true
  def mount(_params, _session, socket) do
    # recents: nil until the hook reports (render nothing), then a list.
    {:ok, assign(socket, recents: nil)}
  end

  @impl true
  def handle_event("create", _params, socket) do
    case Boards.create_board() do
      {:ok, board} -> {:noreply, push_navigate(socket, to: ~p"/boards/#{board.id}")}
      {:error, _} -> {:noreply, socket}
    end
  end

  def handle_event("create_sample", _params, socket) do
    case Boards.create_sample_board() do
      {:ok, board} -> {:noreply, push_navigate(socket, to: ~p"/boards/#{board.id}")}
      {:error, _} -> {:noreply, socket}
    end
  end

  # The .RecentBoards hook reports the localStorage id list. It's untrusted
  # input: keep only well-formed uuids (capped) and read the boards from the
  # DB, so deleted boards drop out.
  def handle_event("recent_boards", %{"boards" => ids}, socket) when is_list(ids) do
    ids =
      ids
      |> Enum.take(10)
      |> Enum.flat_map(fn id ->
        case is_binary(id) && Ecto.UUID.cast(id) do
          {:ok, id} -> [id]
          _ -> []
        end
      end)

    {:noreply, assign(socket, recents: Boards.existing_boards(ids))}
  end

  def handle_event("recent_boards", _malformed, socket) do
    {:noreply, assign(socket, recents: [])}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div style="min-height:100vh;background:#0b1220;color:#e5e7eb;font-family:sans-serif;padding:2em">
      <.flash kind={:error} flash={@flash} />
      <div style="max-width:640px;margin:0 auto">
        <h1 style="font-size:1.8em;font-weight:700;color:#f9fafb;margin:0 0 1em">Kanban boards</h1>

        <div style="display:flex;gap:0.6em;margin-bottom:2em">
          <button
            phx-click="create"
            style="background:#2563eb;color:#fff;border:none;border-radius:6px;padding:0.6em 1.1em;font-size:1em;cursor:pointer"
          >
            Create new board
          </button>
          <button
            phx-click="create_sample"
            style="background:#1f2937;color:#f3f4f6;border:1px solid #374151;border-radius:6px;padding:0.6em 1.1em;font-size:1em;cursor:pointer"
          >
            Generate sample board
          </button>
        </div>

        <h2 style="font-size:1.1em;font-weight:600;color:#9ca3af;margin:0 0 0.7em">
          Recent boards
        </h2>
        <div id="recent-boards" phx-hook=".RecentBoards">
          <div :if={@recents == []} style="color:#6b7280;font-style:italic">
            No recent boards — boards you open show up here.
          </div>
          <ul style="list-style:none;padding:0;margin:0;display:flex;flex-direction:column;gap:0.5em">
            <li :for={board <- @recents || []}>
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
      <script :type={Phoenix.LiveView.ColocatedHook} name=".RecentBoards">
        export default {
          mounted() {
            let stored = null;
            try {
              stored = JSON.parse(localStorage.getItem("llv-kanban:recent-boards"));
            } catch {}
            this.pushEvent("recent_boards", { boards: Array.isArray(stored) ? stored : [] });
          },
        };
      </script>
    </div>
    """
  end
end
