defmodule Local.BoardNameComponent do
  use Phoenix.LiveComponent

  @impl true
  def mount(socket) do
    {:ok, assign(socket, renaming: false)}
  end

  @impl true
  def handle_event("start_rename", _params, socket) do
    {:noreply, assign(socket, :renaming, true)}
  end

  def handle_event("cancel_rename", _params, socket) do
    {:noreply, assign(socket, :renaming, false)}
  end

  def handle_event("rename_board", %{"name" => name}, socket) do
    case String.trim(name) do
      "" -> :ok
      name -> send(self(), {:rename_board, name})
    end

    {:noreply, assign(socket, :renaming, false)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div style="display:flex;align-items:center;gap:0.6em;margin:0 0 0.75em">
      <form
        :if={@renaming}
        phx-submit="rename_board"
        phx-target={@myself}
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
          phx-target={@myself}
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
        phx-target={@myself}
        title="Rename board"
        style="background:none;color:#6b7280;border:none;font-size:1.1em;cursor:pointer;padding:0.2em"
      >
        ✎
      </button>
    </div>
    """
  end
end
