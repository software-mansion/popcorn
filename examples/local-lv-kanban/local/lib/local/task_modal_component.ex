defmodule Local.TaskModalComponent do
  use Phoenix.LiveComponent

  alias Phoenix.LiveView.JS

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, params: assigns.params, error: nil)
    {:ok, socket}
  end

  @impl true
  def handle_event("submit_task_modal", %{"name" => name} = params, socket) do
    case String.trim(name) do
      "" ->
        {:noreply, assign(socket, :error, "Task name cannot be empty")}

      _ ->
        send(self(), {:add_task, params})
        {:noreply, assign(socket, :error, nil)}
    end
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      <div
        :if={@params}
        phx-click="close_task_modal"
        style="position:fixed;inset:0;background:rgba(0,0,0,0.6);z-index:50"
      >
      </div>
      <div
        :if={@params}
        role="dialog"
        aria-modal="true"
        style="position:fixed;top:50%;left:50%;transform:translate(-50%,-50%);background:#1f2937;border:1px solid #374151;border-radius:8px;padding:1.2em;width:min(420px,92vw);z-index:51;color:#e5e7eb;font-family:sans-serif;box-shadow:0 12px 40px rgba(0,0,0,0.5)"
      >
        <div style="display:flex;align-items:center;justify-content:space-between;margin-bottom:0.9em">
          <div style="font-size:1.05em;font-weight:600;color:#f9fafb">
            Add task to <span style="color:#93c5fd">{@params.column_name}</span>
          </div>
          <button
            type="button"
            phx-click="close_task_modal"
            title="Close"
            style="background:transparent;color:#9ca3af;border:none;cursor:pointer;font-size:1.2em;line-height:1;padding:0.1em 0.35em;border-radius:4px"
          >×</button>
        </div>

        <form
          phx-submit="submit_task_modal"
          phx-target={@myself}
          style="display:flex;flex-direction:column;gap:0.75em"
          autocomplete="off"
        >
          <input type="hidden" name="column_id" value={@params.column_id} />

          <label style="display:flex;flex-direction:column;gap:0.3em;font-size:0.85em;color:#cbd5e1">
            Name
            <input
              type="text"
              name="name"
              autocomplete="off"
              phx-mounted={JS.focus()}
              placeholder="Task name"
              style={"background:#111827;color:#f3f4f6;border:1px solid #{if @error, do: "#dc2626", else: "#374151"};border-radius:5px;padding:0.5em 0.6em;font-size:0.95em;outline:none"}
            />
            <span :if={@error} style="color:#fca5a5;font-size:0.8em">{@error}</span>
          </label>

          <label style="display:flex;flex-direction:column;gap:0.3em;font-size:0.85em;color:#cbd5e1">
            Description
            <textarea
              name="description"
              rows="3"
              placeholder="Optional details..."
              style="background:#111827;color:#f3f4f6;border:1px solid #374151;border-radius:5px;padding:0.5em 0.6em;font-size:0.9em;outline:none;font-family:inherit;resize:vertical"
            />
          </label>

          <div style="display:flex;justify-content:flex-end;gap:0.5em;margin-top:0.3em">
            <button
              type="button"
              phx-click="close_task_modal"
              style="background:transparent;color:#cbd5e1;border:1px solid #4b5563;border-radius:5px;padding:0.5em 0.95em;font-size:0.9em;cursor:pointer"
            >Cancel</button>
            <button
              type="submit"
              style="background:#2563eb;color:#fff;border:none;border-radius:5px;padding:0.5em 1.05em;font-size:0.9em;cursor:pointer"
            >Add task</button>
          </div>
        </form>
      </div>
    </div>
    """
  end
end
