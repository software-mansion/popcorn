defmodule LocalLvKanban.Boards do
  @moduledoc """
  Persistence + edit operations for collaborative kanban boards.

  The server is authoritative: each edit is applied to the DB here, and the host
  `BoardLive` broadcasts so every connected client re-reads the board.

  Task `position`s are fractional-index string ranks **generated on the client**
  (`Local.Rank`): add and move both send the literal `position`, and the server
  just persists it. Each task's own id (dash-stripped) is baked onto the end of
  its rank, making every `position` globally unique, so tasks are read ordered by
  `position` alone — two simultaneous moves into the same slot stay distinct and
  deterministically ordered.

  Column `position`s (integers) are also client-generated: `add_column` stores the
  position the client sends and `remove_column` just deletes, so the server never
  renumbers. Positions may have gaps after deletes — fine for ordering — and are
  read with an `id` tiebreak so equal positions stay deterministic.
  """
  import Ecto.Query

  alias LocalLvKanban.Repo
  alias LocalLvKanban.Boards.{Board, Column, Task}

  def list_boards do
    Repo.all(from b in Board, order_by: [desc: b.inserted_at])
  end

  def get_board!(id) do
    Repo.get!(Board, id) |> Repo.preload(columns: [:tasks])
  end

  @doc """
  Like `get_board!/1` but returns `nil` when the board doesn't exist (e.g. it was
  removed), so callers can redirect instead of crashing on a stale URL.
  """
  def get_board(id) do
    case Repo.get(Board, id) do
      nil -> nil
      board -> Repo.preload(board, columns: [:tasks])
    end
  end

  def create_board(attrs \\ %{}) do
    name =
      case attrs |> Map.get("name", "") |> to_string() |> String.trim() do
        "" -> "Untitled board"
        name -> name
      end

    Repo.transaction(fn ->
      board = Repo.insert!(%Board{name: name})

      ["To Do", "In Progress", "Done"]
      |> Enum.with_index()
      |> Enum.each(fn {col_name, i} ->
        Repo.insert!(%Column{board_id: board.id, name: col_name, position: i})
      end)

      board
    end)
  end

  @doc """
  Turns a preloaded `%Board{}` into the JSON-serializable shape the
  `Local.Kanban` popconent expects: columns and tasks each carrying their
  `position` so the client can sort them on render.
  """
  def board_to_data(%Board{} = board) do
    Enum.map(board.columns, fn col ->
      col
      |> Map.take([:id, :name, :position])
      |> Map.put(
        :tasks,
        Enum.map(col.tasks, &Map.take(&1, [:id, :text, :description, :position]))
      )
    end)
  end

  ## Edits — each returns :ok | :error (the caller only needs to know whether to
  ## broadcast or roll back). Removes/moves are idempotent (already-gone => :ok)
  ## so a doubly-applied edit never errors.

  def add_column(board_id, %{"id" => id, "name" => name, "position" => position}) do
    # The client generated the id and position; persist them verbatim so the
    # optimistic column and this authoritative one converge.
    %Column{id: id}
    |> Column.changeset(%{board_id: board_id, name: name, position: position})
    |> Repo.insert()
    |> to_status()
  end

  def add_task(
        board_id,
        %{"column_id" => cid, "text" => text, "id" => id, "position" => pos} = params
      ) do
    if board_column?(board_id, cid) do
      # The client generated the id and position; persist them verbatim so the
      # optimistic row and this authoritative one converge.
      %Task{id: id}
      |> Task.changeset(%{
        column_id: cid,
        text: text,
        description: Map.get(params, "description") || "",
        position: pos
      })
      |> Repo.insert()
      |> to_status()
    else
      :ok
    end
  end

  # Single scoped DELETE (no SELECT first): deleting 0 rows is a no-op, not an
  # error, so a concurrent double-remove can't raise Ecto.StaleEntryError.
  def remove_column(board_id, %{"id" => id}) do
    Repo.delete_all(from c in Column, where: c.id == ^id and c.board_id == ^board_id)
    :ok
  end

  # Scope by board only (not the claimed column): a task belongs to exactly one
  # column, so the board membership is the authorization that matters.
  def remove_task(board_id, %{"task_id" => tid}) do
    Repo.delete_all(
      from t in Task,
        join: c in Column,
        on: c.id == t.column_id,
        where: t.id == ^tid and c.board_id == ^board_id
    )

    :ok
  end

  def move_task(board_id, %{"task_id" => tid, "to_column_id" => dst, "position" => position}) do
    if board_task?(board_id, tid) do
      # Write the client-computed `position` (and column) on this one row, but only
      # if the destination column still exists on this board.
      {count, _} =
        Repo.update_all(
          from(t in Task,
            where:
              t.id == ^tid and
                exists(from c in Column, where: c.id == ^dst and c.board_id == ^board_id)
          ),
          set: [column_id: dst, position: position]
        )

      if count == 1, do: :ok, else: :error
    else
      # The task is already gone — idempotent no-op.
      :ok
    end
  end

  # Collapse a Repo {:ok, _} | {:error, _} result to the edit status the caller needs.
  defp to_status({:ok, _}), do: :ok
  defp to_status({:error, _}), do: :error

  ## Membership checks scoped to the board (so edits can't cross board boundaries).

  defp board_column?(board_id, cid) do
    Repo.exists?(from c in Column, where: c.id == ^cid and c.board_id == ^board_id)
  end

  defp board_task?(board_id, tid) do
    Repo.exists?(
      from t in Task,
        join: c in Column,
        on: c.id == t.column_id,
        where: t.id == ^tid and c.board_id == ^board_id
    )
  end
end
