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

  @doc """
  The subset of `ids` that still exist, as boards in the order the ids were
  given. Used to validate the browser-supplied "recent boards" list: unknown or
  deleted ids silently drop out and names come from the DB, not the client.
  """
  def existing_boards(ids) do
    found = Repo.all(from b in Board, where: b.id in ^ids) |> Map.new(&{&1.id, &1})
    ids |> Enum.map(&found[&1]) |> Enum.reject(&is_nil/1)
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

  # Sample-board templates: `{name, [{column, [task | {task, description}]}]}`.
  @samples [
    {"Product Roadmap",
     [
       {"Backlog",
        [
          {"Dark mode", "Add a system-aware dark theme toggle in settings."},
          {"Mobile layout", "Responsive board view for phones and tablets."},
          "Keyboard shortcuts"
        ]},
       {"To Do", [{"Drag-and-drop polish", "Smooth out card reordering animations."}]},
       {"In Progress", [{"Realtime sync", "Reconcile concurrent edits via PubSub broadcast."}]},
       {"Done", ["Database-backed boards", "Server-authoritative edits"]}
     ]},
    {"Weekend Trip",
     [
       {"Ideas", ["Kayaking on the lake", {"Old town food tour", "Book a table for Saturday."}]},
       {"Booked", [{"Cabin by the forest", "Check-in Friday 4pm."}]},
       {"Packed", ["Hiking boots", "Rain jacket"]}
     ]},
    {"Sprint 24",
     [
       {"To Do", [{"Fix flaky e2e test", "The move_task test intermittently fails."}]},
       {"In Progress",
        [{"Optimistic UI rebase", "Replay pending ops after authoritative push."}]},
       {"Review", ["PR: contiguous position renumbering"]},
       {"Done", ["Set up CI pipeline", "Add board seeds"]}
     ]}
  ]

  @doc """
  Creates a randomly-picked pre-filled sample board (columns + tasks), for the
  index page's "generate sample board" button.
  """
  def create_sample_board do
    {name, columns} = Enum.random(@samples)

    Repo.transaction(fn ->
      board = Repo.insert!(%Board{name: name})

      columns
      |> Enum.with_index()
      |> Enum.each(fn {{col_name, tasks}, col_pos} ->
        column = Repo.insert!(%Column{board_id: board.id, name: col_name, position: col_pos})

        tasks
        |> Enum.with_index()
        |> Enum.each(fn {task, task_pos} ->
          {text, description} =
            case task do
              {text, description} -> {text, description}
              text -> {text, ""}
            end

          id = Ecto.UUID.generate()

          Repo.insert!(%Task{
            id: id,
            column_id: column.id,
            text: text,
            description: description,
            position: sample_position(task_pos, id)
          })
        end)
      end)

      board
    end)
  end

  # A server-generated task position: fixed-width base-36 index (lexicographic
  # order matches insertion order) with the id baked on — the same string shape
  # the client generates at runtime (`Local.Rank`).
  defp sample_position(index, id) do
    String.pad_leading(Integer.to_string(index, 36), 3, "0") <> String.replace(id, "-", "")
  end

  def board_to_data(%Board{} = board) do
    Map.new(board.columns, fn col ->
      {col.id,
       col
       |> Map.take([:id, :name, :position])
       |> Map.put(
         :tasks,
         Map.new(col.tasks, &{&1.id, Map.take(&1, [:id, :text, :description, :position])})
       )}
    end)
  end

  ## Edits — each returns :ok | :error (the caller only needs to know whether to
  ## broadcast or roll back). Removes/moves are idempotent (already-gone => :ok)
  ## so a doubly-applied edit never errors.

  # Missing board (removed concurrently) => idempotent :ok; an invalid name
  # (empty / too long) => :error, so the origin client rolls back.
  def rename_board(board_id, %{"name" => name}) do
    case Repo.get(Board, board_id) do
      nil ->
        :ok

      board ->
        board
        |> Board.changeset(%{name: name})
        |> Repo.update()
        |> to_status()
    end
  end

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
