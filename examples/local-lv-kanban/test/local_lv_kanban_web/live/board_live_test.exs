defmodule LocalLvKanbanWeb.BoardLiveTest do
  use LocalLvKanbanWeb.ConnCase

  import Phoenix.LiveViewTest

  alias LocalLvKanban.Boards

  setup do
    {:ok, board} = Boards.create_board(%{"name" => "Test Board"})
    %{board: board}
  end

  test "GET /boards/:id renders the kanban mount point seeded with the board",
       %{conn: conn, board: board} do
    {:ok, _lv, html} = live(conn, ~p"/boards/#{board.id}")

    # The local component is mounted as a Popcorn mount point...
    assert html =~ ~s(data-pop-view="Local.Kanban")

    # ...and the board (the columns seeded by create_board/1) is serialized into
    # its assigns.
    assert html =~ "data-pop-assigns"
    assert html =~ "To Do"
    assert html =~ "In Progress"
  end

  test "mount point id is stable across dead and connected renders",
       %{conn: conn, board: board} do
    # The mount point lives under phx-update="ignore"; a random id that differs
    # between the dead and connected render breaks morphdom and the runtime never
    # reads the serialized assigns. The id must be deterministic.
    dead = conn |> get(~p"/boards/#{board.id}") |> html_response(200)
    {:ok, _lv, connected} = live(conn, ~p"/boards/#{board.id}")

    [_, dead_id] = Regex.run(~r/id="(llv-[^"]*)"/, dead)
    [_, connected_id] = Regex.run(~r/id="(llv-[^"]*)"/, connected)

    assert dead_id == connected_id
  end
end
