defmodule Local.ColumnComponent do
  use Phoenix.Component

  alias Local.TaskComponent

  @placeholder_height 36

  def placeholder_height, do: @placeholder_height

  attr :col, :map, required: true
  attr :target, :string, required: true
  attr :dragging, :map, default: nil
  attr :drag_target, :map, default: nil

  def column(assigns) do
    assigns =
      assign(assigns, :tasks, assigns.col.tasks |> Map.values() |> Enum.sort_by(& &1.position))

    ~H"""
    <div
      phx-dragover="drag_over_column"
      phx-value-column_id={@col.id}
      style={"flex:0 0 280px;background:#1f2937;border:1px solid #{if @dragging, do: "#4b6bb3", else: "#374151"};border-radius:8px;padding:0.75em;display:flex;flex-direction:column;gap:0.6em"}
    >
      <div style="display:flex;align-items:center;justify-content:space-between;gap:0.5em">
        <div style="font-weight:600;font-size:1em;color:#f3f4f6">
          {@col.name}
          <span style="color:#9ca3af;font-weight:400;font-size:0.85em">({length(@tasks)})</span>
        </div>
        <button
          type="button"
          phx-click="remove_column"
          phx-target={@target}
          phx-value-id={@col.id}
          title="Remove column"
          style="background:transparent;color:#9ca3af;border:none;cursor:pointer;font-size:1.1em;line-height:1;padding:0.1em 0.35em;border-radius:4px"
        >×</button>
      </div>

      <div style="display:flex;flex-direction:column;gap:0.4em">
        <%= for task <- @tasks do %>
          <.placeholder :if={@dragging && placeholder_here?(@drag_target, @col.id, task.id)} />
          <TaskComponent.card
            task={task}
            column_id={@col.id}
            target={@target}
            is_dragging={dragging_task?(@dragging, task.id)}
          />
        <% end %>
        <%!-- End-of-column placeholder. Shows only when the target is the end
              (e.g. hovering the column outside any task), so it never reserves
              empty space otherwise. --%>
        <.placeholder :if={@dragging && placeholder_here?(@drag_target, @col.id, nil)} />
        <div :if={is_nil(@dragging) and @tasks == []} style="color:#6b7280;font-size:0.85em;font-style:italic;padding:0.3em 0.2em">
          No tasks yet
        </div>
      </div>

      <button
        type="button"
        phx-click="open_task_modal"
        phx-value-column_id={@col.id}
        style="background:#2563eb;color:#fff;border:none;border-radius:5px;padding:0.5em 0.7em;font-size:0.85em;cursor:pointer;margin-top:0.2em"
      >+ Add task</button>
    </div>
    """
  end

  defp placeholder(assigns) do
    ~H"""
    <div style="background:rgba(75,107,179,0.15);border:2px dashed #4b6bb3;border-radius:6px;min-height:36px;pointer-events:none"></div>
    """
  end

  defp placeholder_here?(%{column_id: c, before_task_id: t}, c, t), do: true
  defp placeholder_here?(_, _, _), do: false

  defp dragging_task?(%{task_id: tid}, tid), do: true
  defp dragging_task?(_, _), do: false
end
