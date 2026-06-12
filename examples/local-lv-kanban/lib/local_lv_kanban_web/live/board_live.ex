defmodule LocalLvKanbanWeb.BoardLive do
  @moduledoc """
  Server LiveView that hosts the Kanban `LocalComponent`.

  The board's initial state is produced here, on the server, and handed to the
  client-side `Local.Kanban` component as an assign via the `<.local_component>`
  component. From there the local component runs entirely in the browser
  (Popcorn/WASM).
  """
  use LocalLvKanbanWeb, :live_view

  @impl true
  def mount(_params, _session, socket) do
    {:ok, assign(socket, :board, initial_board())}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div style="font-size:1.5em;font-family:sans-serif;padding:1em;color:#fcfcfc">
      <.local_component module="Local.Kanban" board={@board} />
    </div>
    """
  end

  # The seed board, as plain JSON-serializable data. The component serializes it
  # into the mount point; `Local.Kanban` assigns ids and builds its ordered maps
  # from it in the browser when its update/2 receives the board assign.
  defp initial_board do
    [
      %{
        name: "To Do",
        tasks: [
          %{text: "Design landing page", description: ""},
          %{text: "Write project proposal", description: "Outline scope, timeline, budget."},
          %{text: "Set up CI/CD pipeline", description: ""},
          %{text: "Measure performance", description: ""}
        ]
      },
      %{
        name: "In Progress",
        tasks: [
          %{text: "Implement auth flow", description: "OAuth + session handling."},
          %{text: "Refactor database layer", description: ""}
        ]
      },
      %{
        name: "Review",
        tasks: [%{text: "Code review for PR #42", description: ""}]
      },
      %{
        name: "Done",
        tasks: [
          %{text: "Set up development environment", description: ""},
          %{text: "Initial repo structure", description: ""}
        ]
      }
    ]
  end
end
