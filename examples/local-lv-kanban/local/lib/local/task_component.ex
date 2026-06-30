defmodule Local.TaskComponent do
  use Phoenix.Component

  attr :task, :map, required: true
  attr :column_id, :string, required: true
  attr :target, :string, required: true
  attr :is_dragging, :boolean, default: false

  def card(assigns) do
    ~H"""
    <div
      id={"task-#{@task.id}"}
      draggable="true"
      phx-dragstart="drag_start"
      phx-dragover="drag_over_task"
      phx-dragend="drag_end"
      phx-value-column_id={@column_id}
      phx-value-task_id={@task.id}
      style={task_style(@is_dragging)}
    >
      <div style="flex:1;word-break:break-word;color:#e5e7eb;display:flex;flex-direction:column;gap:0.2em;min-width:0;pointer-events:none">
        <span>{@task.text}</span>
        <span :if={@task.description != ""} style="color:#9ca3af;font-size:0.85em;font-style:italic">{@task.description}</span>
      </div>
      <button
        type="button"
        draggable="false"
        phx-click="remove_task"
        phx-target={@target}
        phx-value-column_id={@column_id}
        phx-value-task_id={@task.id}
        title="Remove task"
        style="background:transparent;color:#9ca3af;border:none;cursor:pointer;font-size:0.95em;line-height:1;padding:0 0.25em;border-radius:4px"
      >×</button>
    </div>
    """
  end

  defp task_style(false),
    do:
      "background:#374151;border:1px solid #4b5563;border-radius:6px;padding:0.5em 0.6em;display:flex;align-items:flex-start;justify-content:space-between;gap:0.5em;font-size:0.9em;cursor:grab;user-select:none"

  defp task_style(true),
    do:
      "background:#374151;border:1px solid #4b5563;border-radius:6px;padding:0.5em 0.6em;display:flex;align-items:flex-start;justify-content:space-between;gap:0.5em;font-size:0.9em;cursor:grabbing;user-select:none;opacity:0.4"
end
