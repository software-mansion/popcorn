defmodule LocalLvKanbanWeb.BoardsLiveTest do
  use LocalLvKanbanWeb.ConnCase

  import Phoenix.LiveViewTest

  alias LocalLvKanban.Boards

  test "'Create new board' creates a fresh board and navigates to it", %{conn: conn} do
    {:ok, lv, _html} = live(conn, ~p"/")

    lv |> element("button", "Create new board") |> render_click()

    {path, _flash} = assert_redirect(lv)
    assert "/boards/" <> id = path

    board = Boards.get_board!(id)
    assert board.name == "Untitled board"
    assert Enum.all?(board.columns, &(&1.tasks == []))
  end

  test "'Generate sample board' creates a pre-filled board and navigates to it", %{conn: conn} do
    {:ok, lv, _html} = live(conn, ~p"/")

    lv |> element("button", "Generate sample board") |> render_click()

    {path, _flash} = assert_redirect(lv)
    assert "/boards/" <> id = path
    assert Enum.any?(Boards.get_board!(id).columns, &(&1.tasks != []))
  end

  # The hook pushes whatever localStorage held; the server must keep only boards
  # that still exist (client order preserved) and ignore garbage entries.
  test "recent boards render only ids that still exist, in the reported order", %{conn: conn} do
    {:ok, one} = Boards.create_board(%{"name" => "One"})
    {:ok, two} = Boards.create_board(%{"name" => "Two"})
    deleted_id = Ecto.UUID.generate()

    {:ok, lv, html} = live(conn, ~p"/")
    # Nothing is rendered (not even the empty state) until the hook reports.
    refute html =~ "No recent boards"

    html =
      render_hook(lv, "recent_boards", %{
        "boards" => [two.id, deleted_id, one.id, "not-a-uuid", 42]
      })

    refute html =~ deleted_id
    assert html =~ two.id
    assert html =~ one.id
    # "Two" was reported first (most recent), so it renders before "One".
    {pos_two, _} = :binary.match(html, two.id)
    {pos_one, _} = :binary.match(html, one.id)
    assert pos_two < pos_one
  end

  test "an empty recents report shows the empty state", %{conn: conn} do
    {:ok, lv, _html} = live(conn, ~p"/")

    assert render_hook(lv, "recent_boards", %{"boards" => []}) =~ "No recent boards"
    assert render_hook(lv, "recent_boards", %{"boards" => "garbage"}) =~ "No recent boards"
  end
end
