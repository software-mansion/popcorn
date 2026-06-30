defmodule LocalLvKanban.BoardsTest do
  use LocalLvKanban.DataCase, async: true

  alias LocalLvKanban.Boards
  alias LocalLvKanban.Boards.Task

  # Positions are generated on the client now; the server just persists what it
  # receives. These tests cover that contract: add/move store the given id and
  # position verbatim, a move touches only the moved row, and ordering follows
  # `position`. The fractional-rank generation itself lives in `Local.Rank` and
  # is exercised end-to-end by the Playwright suite.

  describe "add_task" do
    test "persists the client-provided id and position" do
      {board, todo, _ip, _done} = setup_board()
      a = add_task(board, todo, "A", "a")
      _b = add_task(board, todo, "B", "b")

      assert ordered_texts(todo.id) == ["A", "B"]
      # The row uses the client's id and stores the given position verbatim.
      assert Repo.get(Task, a).position == "a" <> strip(a)
    end

    test "a missing column surfaces as an error, not a raised ConstraintError" do
      # Mirrors the race where the column is deleted between add_task's
      # board_column check and the insert: the FK violation must degrade to
      # {:error, changeset} (foreign_key_constraint), not raise.
      assert {:error, changeset} =
               %Task{id: Ecto.UUID.generate()}
               |> Task.changeset(%{
                 "column_id" => Ecto.UUID.generate(),
                 "text" => "X",
                 "position" => "a"
               })
               |> Repo.insert()

      assert changeset.errors[:column_id]
    end
  end

  describe "move_task" do
    test "stores the given position on only the moved row" do
      {board, todo, _ip, _done} = setup_board()
      a = add_task(board, todo, "A", "a")
      b = add_task(board, todo, "B", "b")
      c = add_task(board, todo, "C", "c")

      before = positions([a, b])

      # Move C to the front: the client computed a rank that sorts before "a".
      assert :ok = move_task(board, c, todo, "0" <> strip(c))

      assert ordered_texts(todo.id) == ["C", "A", "B"]
      # A and B are untouched — only the moved row is written.
      assert positions([a, b]) == before
    end

    test "moves across columns, updating only the moved row" do
      {board, todo, _ip, done} = setup_board()
      a = add_task(board, todo, "A", "a")
      b = add_task(board, todo, "B", "b")
      before = positions([a])

      assert :ok = move_task(board, b, done, "a" <> strip(b))

      assert ordered_texts(todo.id) == ["A"]
      assert ordered_texts(done.id) == ["B"]
      assert positions([a]) == before
    end

    test "is a noop for a task that is not on the board" do
      {board, _todo, _ip, done} = setup_board()

      assert :ok =
               Boards.move_task(board.id, %{
                 "task_id" => Ecto.UUID.generate(),
                 "to_column_id" => done.id,
                 "position" => "a"
               })
    end

    test "errors (no FK crash) when the destination column was removed" do
      {board, todo, _ip, done} = setup_board()
      a = add_task(board, todo, "A", "a")
      :ok = Boards.remove_column(board.id, %{"id" => done.id})

      assert :error = move_task(board, a, done, "b" <> strip(a))
      # The task stays put.
      assert ordered_texts(todo.id) == ["A"]
    end
  end

  ## Helpers

  defp setup_board do
    {:ok, board} = Boards.create_board(%{"name" => "Test"})
    board = Boards.get_board!(board.id)
    [todo, ip, done] = board.columns
    {board, todo, ip, done}
  end

  # Mimics the client: generate an id, bake it into the rank, send both. Returns
  # the id (add_task returns just :ok).
  defp add_task(board, column, text, rank) do
    id = Ecto.UUID.generate()

    :ok =
      Boards.add_task(board.id, %{
        "column_id" => column.id,
        "text" => text,
        "id" => id,
        "position" => rank <> strip(id)
      })

    id
  end

  defp move_task(board, task_id, dst_column, position) do
    Boards.move_task(board.id, %{
      "task_id" => task_id,
      "to_column_id" => dst_column.id,
      "position" => position
    })
  end

  defp strip(id), do: String.replace(id, "-", "")

  defp positions(ids) do
    Repo.all(from t in Task, where: t.id in ^ids, select: {t.id, t.position}) |> Map.new()
  end

  defp ordered_texts(column_id) do
    Repo.all(
      from t in Task,
        where: t.column_id == ^column_id,
        order_by: [asc: t.position],
        select: t.text
    )
  end
end
