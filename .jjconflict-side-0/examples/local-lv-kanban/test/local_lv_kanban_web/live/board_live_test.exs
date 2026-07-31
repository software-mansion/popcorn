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
    # its assigns. The payload is Base.encode64(:erlang.term_to_binary(...)), so
    # decode it the way the runtime does before asserting on the column names.
    assert html =~ "data-pop-assigns"

    column_names = for {_id, col} <- decode_assigns(html).board, do: col.name
    assert "To Do" in column_names
    assert "In Progress" in column_names
  end

  defp decode_assigns(html) do
    [_, encoded] = Regex.run(~r/data-pop-assigns="([^"]*)"/, html)
    encoded |> Base.decode64!() |> :erlang.binary_to_term()
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
