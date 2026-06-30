defmodule Local.Kanban do
  use LocalLiveView.Popconent

  alias Local.ColumnComponent
  alias Local.OrderedMap

  # The board's columns, and each column's tasks, are `OrderedMap`s keyed by id —
  # so iteration order is the on-screen order and lookups/updates go through the
  # Access key path (`columns[cid][:tasks][tid]`). Task reordering is a splice
  # (`OrderedMap.insert_before/4`); columns are only appended or removed.

  @impl true
  def mount(socket) do
    {:ok, assign(socket, task_modal: nil, dragging: nil, drag_target: nil)}
  end

  @impl true
  def update(%{board: board}, socket) do
    {:ok, assign(socket, :columns, build_columns(board))}
  end

  # --- Columns & tasks -------------------------------------------------------

  @impl true
  def handle_event("remove_column", %{"id" => id}, socket) do
    {_column, columns} = pop_in(socket.assigns.columns, [id])
    {:noreply, assign(socket, :columns, columns)}
  end

  def handle_event("remove_task", %{"column_id" => cid, "task_id" => tid}, socket) do
    columns = update_in(socket.assigns.columns, [cid, :tasks], &OrderedMap.delete(&1, tid))
    {:noreply, assign(socket, :columns, columns)}
  end

  # --- Task modal ------------------------------------------------------------

  def handle_event("open_task_modal", %{"column_id" => cid}, socket) do
    case socket.assigns.columns[cid] do
      nil ->
        {:noreply, socket}

      column ->
        {:noreply, assign(socket, :task_modal, %{column_id: cid, column_name: column.name})}
    end
  end

  def handle_event("close_task_modal", _params, socket) do
    {:noreply, assign(socket, :task_modal, nil)}
  end

  # --- Drag & drop -----------------------------------------------------------

  def handle_event("drag_start", %{"column_id" => cid, "task_id" => tid}, socket) do
    # get_in is nil-safe, so this validates that both the column and task exist.
    if get_in(socket.assigns.columns, [cid, :tasks, tid]) do
      {:noreply,
       assign(socket, dragging: %{task_id: tid, source_column_id: cid}, drag_target: nil)}
    else
      {:noreply, socket}
    end
  end

  def handle_event("drag_over_task", %{"column_id" => cid, "task_id" => tid} = params, socket) do
    case socket.assigns.dragging do
      # Hovering the dragged card itself — nothing to do.
      %{task_id: ^tid} ->
        {:noreply, socket}

      %{} = dragging ->
        before_id = insertion_point(socket.assigns.columns, dragging, cid, tid, params)
        target = resolve_target(socket.assigns.columns, dragging, cid, before_id)
        {:noreply, assign(socket, :drag_target, target)}

      nil ->
        {:noreply, socket}
    end
  end

  def handle_event("drag_over_column", %{"column_id" => cid}, socket) do
    # Skip when not dragging, or already targeting this column via a task (so
    # brushing the gaps between cards doesn't snap the placeholder to the end).
    with %{} = dragging <- socket.assigns.dragging,
         false <- match?(%{column_id: ^cid}, socket.assigns.drag_target) do
      target = resolve_target(socket.assigns.columns, dragging, cid, nil)
      {:noreply, assign(socket, :drag_target, target)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("drag_end", _params, socket) do
    socket =
      case {socket.assigns.dragging, socket.assigns.drag_target} do
        {%{task_id: tid, source_column_id: src}, %{column_id: dst, before_task_id: before_id}} ->
          assign(socket, :columns, move_task(socket.assigns.columns, src, tid, dst, before_id))

        _ ->
          socket
      end

    {:noreply, assign(socket, dragging: nil, drag_target: nil)}
  end

  @impl true
  def handle_info({:add_column, name}, socket) do
    id = uuid()
    column = %{id: id, name: name, tasks: OrderedMap.new()}
    {:noreply, assign(socket, :columns, put_in(socket.assigns.columns, [id], column))}
  end

  def handle_info({:add_task, %{"column_id" => cid, "name" => name} = params}, socket) do
    task = %{
      id: uuid(),
      text: String.trim(name),
      description: params |> Map.get("description", "") |> String.trim()
    }

    columns = update_in(socket.assigns.columns, [cid, :tasks], &OrderedMap.put(&1, task.id, task))

    {:noreply, assign(socket, columns: columns, task_modal: nil)}
  end

  # --- Drag helpers ----------------------------------------------------------

  # Where to insert when hovering `tid`: before the task when the cursor is in its
  # top half, after it otherwise.
  defp insertion_point(columns, dragging, cid, tid, params) do
    offset_y = params["clientY"] - params["rect"]["top"]

    if offset_y < params["rect"]["height"] / 2 do
      tid
    else
      # Insert after the hovered task, skipping the dragged card when it sits
      # right there (it is about to leave this slot).
      dragged = dragging.task_id

      case successor_id(columns, cid, tid) do
        ^dragged -> successor_id(columns, cid, dragged)
        after_id -> after_id
      end
    end
  end

  # A same-column drop onto the card's own slot (its original successor, or the
  # end when it is already last) is a no-op, so drop the placeholder entirely.
  defp resolve_target(columns, %{task_id: tid, source_column_id: src}, cid, before_id) do
    if cid == src and before_id == successor_id(columns, src, tid) do
      nil
    else
      %{column_id: cid, before_task_id: before_id}
    end
  end

  defp move_task(columns, src_id, task_id, dst_id, before_id) do
    task = get_in(columns, [src_id, :tasks, task_id])

    columns
    |> update_in([src_id, :tasks], &OrderedMap.delete(&1, task_id))
    |> update_in([dst_id, :tasks], &OrderedMap.insert_before(&1, task.id, task, before_id))
  end

  defp successor_id(columns, cid, tid), do: OrderedMap.next_key(columns[cid].tasks, tid)

  # Turns the JSON board handed down by the server LiveView (string-keyed maps,
  # tasks as plain lists) into the ordered-map structure the rest of the view
  # works with, assigning a fresh id to every column and task.
  defp build_columns(board) do
    board
    |> Enum.map(fn col ->
      tasks =
        Enum.map(col["tasks"], fn task ->
          %{id: uuid(), text: task["text"], description: task["description"] || ""}
        end)

      %{id: uuid(), name: col["name"], tasks: OrderedMap.new(tasks, & &1.id)}
    end)
    |> OrderedMap.new(& &1.id)
  end

  defp uuid do
    <<a::48, _::4, b::12, _::2, c::62>> = :crypto.strong_rand_bytes(16)

    <<g1::binary-8, g2::binary-4, g3::binary-4, g4::binary-4, g5::binary-12>> =
      Base.encode16(<<a::48, 4::4, b::12, 2::2, c::62>>, case: :lower)

    "#{g1}-#{g2}-#{g3}-#{g4}-#{g5}"
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div style={"font-family:sans-serif;color:#e5e7eb;padding:0.5em 0#{if @dragging, do: ";user-select:none"}"}>
      <h1 style="margin:0 0 0.75em;font-size:1.6em;font-weight:600;color:#f9fafb">Kanban Board</h1>

      <div style="display:flex;gap:1em;overflow-x:auto;padding-bottom:1em;align-items:flex-start">
        <ColumnComponent.column
          :for={col <- OrderedMap.values(@columns)}
          col={col}
          dragging={@dragging}
          drag_target={@drag_target}
        />

        <.live_component module={Local.AddColumnComponent} id="add-column" />
      </div>

      <.live_component
        module={Local.TaskModalComponent}
        id="task-modal"
        params={@task_modal}
      />
    </div>
    """
  end
end
