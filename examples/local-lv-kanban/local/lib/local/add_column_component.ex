defmodule Local.AddColumnComponent do
  use Phoenix.Component

  # "Add column" form. Client-only (no phx-target): the parent Kanban applies the
  # add optimistically, generates the column's id + position, and pushes them to
  # the host LiveView itself. `required` blocks empty names client-side; the server
  # validates length too.

  attr :seq, :integer, default: 0

  def add_column(assigns) do
    ~H"""
    <form
      id={"add-column-form-#{@seq}"}
      phx-submit="add_column"
      autocomplete="off"
      style="flex:0 0 280px;background:#111827;border:1px dashed #374151;border-radius:8px;padding:0.75em;display:flex;flex-direction:column;gap:0.5em"
    >
      <div style="font-weight:600;color:#9ca3af;font-size:0.95em">Add column</div>
      <input
        type="text"
        name="name"
        required
        placeholder="Column name..."
        autocomplete="off"
        style="background:#1f2937;color:#f3f4f6;border:1px solid #374151;border-radius:5px;padding:0.45em 0.55em;font-size:0.9em;outline:none"
      />
      <button
        type="submit"
        style="background:#16a34a;color:#fff;border:none;border-radius:5px;padding:0.45em 0.6em;font-size:0.9em;cursor:pointer"
      >Add column</button>
    </form>
    """
  end
end
