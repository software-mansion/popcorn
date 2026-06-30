# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inserts a few demo boards, each with ordered columns and tasks. Ids are fixed
# (see next_uuid below), so re-running is idempotent — `on_conflict: :nothing`
# skips rows that already exist (and leaves any boards you created untouched).
# Wipe with `mix ecto.reset` if you want a clean slate.

alias LocalLvKanban.Repo
alias LocalLvKanban.Boards.{Board, Column, Task}

# Deterministic, hardcoded ids: hand out valid UUIDs from a global counter, in
# insertion order, so re-seeding (after `mix ecto.reset`) always produces the same
# board/column/task ids. That keeps bookmarked board URLs and test fixtures stable
# instead of drifting on every run like `Ecto.UUID.generate/0` would.
next_uuid = fn ->
  n = Process.get(:seed_counter, 0) + 1
  Process.put(:seed_counter, n)
  suffix = n |> Integer.to_string(16) |> String.pad_leading(12, "0")
  "00000000-0000-4000-8000-" <> suffix
end

# A seed task's position: a fixed-width base-36 index (so lexicographic order
# matches insertion order) with the id baked on (so positions are unique) — the
# same string shape the client generates at runtime.
seed_position = fn index, id ->
  String.pad_leading(Integer.to_string(index, 36), 3, "0") <> String.replace(id, "-", "")
end

# A board is `{name, [{column_name, [task_text | {task_text, description}]}]}`.
boards = [
  {"Product Roadmap",
   [
     {"Backlog",
      [
        {"Dark mode", "Add a system-aware dark theme toggle in settings."},
        {"Mobile layout", "Responsive board view for phones and tablets."},
        "Keyboard shortcuts",
        "Export board to CSV"
      ]},
     {"To Do",
      [
        {"Drag-and-drop polish", "Smooth out card reordering animations."},
        "Empty-state illustrations"
      ]},
     {"In Progress",
      [
        {"Realtime sync", "Reconcile concurrent edits via PubSub broadcast."}
      ]},
     {"Done",
      [
        "Database-backed boards",
        "Server-authoritative edits"
      ]}
   ]},
  {"Sprint 24",
   [
     {"To Do",
      [
        {"Fix flaky e2e test", "Playwright move_task test intermittently fails."},
        "Bump dependencies",
        "Write changelog"
      ]},
     {"In Progress",
      [
        {"Optimistic UI rebase", "Replay pending ops after authoritative push."}
      ]},
     {"Review",
      [
        "PR: contiguous position renumbering"
      ]},
     {"Done",
      [
        "Set up CI pipeline",
        "Add board seeds"
      ]}
   ]},
  {"Personal",
   [
     {"Ideas",
      [
        "Learn Elixir LiveView",
        {"Side project: WASM kanban", "Run LiveView popconents client-side."},
        "Read 'Designing Elixir Systems'"
      ]},
     {"Doing",
      [
        "Refactor home network setup"
      ]},
     {"Done",
      [
        "Plant tomatoes",
        "Fix the leaky faucet"
      ]}
   ]}
]

for {board_name, columns} <- boards do
  Repo.transaction(fn ->
    board = Repo.insert!(%Board{id: next_uuid.(), name: board_name}, on_conflict: :nothing)

    columns
    |> Enum.with_index()
    |> Enum.each(fn {{col_name, tasks}, col_pos} ->
      column =
        Repo.insert!(
          %Column{
            id: next_uuid.(),
            board_id: board.id,
            name: col_name,
            position: col_pos
          },
          on_conflict: :nothing
        )

      tasks
      |> Enum.with_index()
      |> Enum.each(fn {task, task_pos} ->
        {text, description} =
          case task do
            {text, description} -> {text, description}
            text -> {text, ""}
          end

        id = next_uuid.()

        Repo.insert!(
          %Task{
            id: id,
            column_id: column.id,
            text: text,
            description: description,
            position: seed_position.(task_pos, id)
          },
          on_conflict: :nothing
        )
      end)
    end)

    board
  end)
end

IO.puts("Seeded #{length(boards)} boards.")
