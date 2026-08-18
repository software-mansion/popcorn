defmodule Local.BoardNameComponent do
  use Phoenix.Component

  # Board title with inline rename. The `renaming` toggle is driven by the
  # parent Kanban's state (start_rename/cancel_rename). The form has no
  # phx-target — Kanban applies the rename optimistically and pushes it to
  # the host LiveView itself.

  attr :name, :string, required: true
  attr :renaming, :boolean, default: false

  def board_name(assigns) do
    ~H"""
    <div style="display:flex;align-items:center;gap:0.6em;margin:0 0 0.75em">
      <form
        :if={@renaming}
        phx-submit="rename_board"
        style="display:flex;gap:0.5em;align-items:center"
        autocomplete="off"
      >
        <input
          type="text"
          name="name"
          value={@name}
          required
          autofocus
          style="background:#1f2937;color:#f3f4f6;border:1px solid #374151;border-radius:6px;padding:0.4em 0.6em;font-size:1.2em;outline:none"
        />
        <button
          type="submit"
          style="background:#2563eb;color:#fff;border:none;border-radius:6px;padding:0.45em 0.9em;font-size:0.95em;cursor:pointer"
        >
          Save
        </button>
        <button
          type="button"
          phx-click="cancel_rename"
          style="background:#1f2937;color:#9ca3af;border:1px solid #374151;border-radius:6px;padding:0.45em 0.9em;font-size:0.95em;cursor:pointer"
        >
          Cancel
        </button>
      </form>
      <h1 :if={!@renaming} style="margin:0;font-size:1.6em;font-weight:600;color:#f9fafb">
        {@name}
      </h1>
      <button
        :if={!@renaming}
        phx-click="start_rename"
        title="Rename board"
        style="background:none;color:#6b7280;border:none;font-size:1.1em;cursor:pointer;padding:0.2em"
      >
        ✎
      </button>
    </div>
    """
  end
end
