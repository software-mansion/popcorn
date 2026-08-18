defmodule Local.Kanban do
  use LocalLiveView

  alias Local.{AddColumnComponent, BoardNameComponent, ColumnComponent, Rank, TaskModalComponent}

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       name: nil,
       board: %{},
       renaming: false,
       task_modal: nil,
       dragging: nil,
       drag_target: nil,
       add_seq: 0
     )}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, name: assigns.name, board: assigns.board)
    board = assigns.board

    is_drag_valid =
      case socket.assigns.dragging do
        %{task_id: tid, source_column_id: cid} -> get_in(board, [cid, :tasks, tid]) != nil
        nil -> true
      end

    is_drag_target_valid =
      case socket.assigns.drag_target do
        %{column_id: cid, before_task_id: nil} -> Map.has_key?(board, cid)
        %{column_id: cid, before_task_id: btid} -> get_in(board, [cid, :tasks, btid]) != nil
        nil -> true
      end

    socket =
      if is_drag_valid and is_drag_target_valid do
        socket
      else
        # Affected task/column was removed by the server update, cancel dragging
        assign(socket, dragging: nil, drag_target: nil)
      end

    {:ok, socket}
  end

  # --- Board & tasks (optimistic) ------------------------------------------

  @impl true
  def handle_event("add_column", %{"name" => name}, socket) do
    case String.trim(name) do
      "" ->
        {:noreply, socket}

      name ->
        # The client owns the position: generate the id + an append position
        # (max + 1) and tell the host to persist them verbatim (reusing the same
        # id, so optimistic and authoritative board converge). Bumping add_seq
        # re-mounts the (uncontrolled) add-column input so it clears — but only
        # after *this* client adds.
        id = uuid()
        position = next_column_position(socket.assigns.board)
        column = %{id: id, name: name, position: position, tasks: %{}}

        {:noreply,
         socket
         |> assign(:board, Map.put(socket.assigns.board, id, column))
         |> assign(:add_seq, socket.assigns.add_seq + 1)
         |> push_server_event("add_column", %{
           "id" => id,
           "name" => name,
           "position" => position
         })}
    end
  end

  def handle_event("add_task", %{"column_id" => cid, "text" => text} = params, socket) do
    case {String.trim(text), socket.assigns.board[cid]} do
      {"", _} ->
        {:noreply, assign(socket, :task_modal, nil)}

      {_text, nil} ->
        {:noreply, assign(socket, :task_modal, nil)}

      {text, column} ->
        # The client owns the position: generate the id up front and an append
        # rank, then tell the host to persist it verbatim (and reuse the same id,
        # so optimistic and authoritative rows converge).
        id = uuid()
        position = Rank.key_before(Map.values(column.tasks), nil, id)

        task = %{
          id: id,
          text: text,
          description: params |> Map.get("description", "") |> String.trim(),
          position: position
        }

        socket =
          socket
          |> assign(:board, put_in(socket.assigns.board, [cid, :tasks, id], task))
          |> assign(:task_modal, nil)
          |> push_server_event("add_task", %{
            "column_id" => cid,
            "text" => task.text,
            "description" => task.description,
            "id" => id,
            "position" => position
          })

        {:noreply, socket}
    end
  end

  def handle_event("remove_column", %{"id" => id} = payload, socket) do
    {_column, board} = pop_in(socket.assigns.board, [id])

    {:noreply,
     socket
     |> assign(:board, board)
     |> push_server_event("remove_column", payload)}
  end

  def handle_event("remove_task", %{"column_id" => cid, "task_id" => tid} = payload, socket) do
    {_task, board} = pop_in(socket.assigns.board, [cid, :tasks, tid])

    {:noreply,
     socket
     |> assign(:board, board)
     |> push_server_event("remove_task", payload)}
  end

  # --- Task modal (local-only UI state) --------------------------------------

  def handle_event("open_task_modal", %{"column_id" => cid}, socket) do
    case socket.assigns.board[cid] do
      nil ->
        {:noreply, socket}

      column ->
        {:noreply, assign(socket, :task_modal, %{column_id: cid, column_name: column.name})}
    end
  end

  def handle_event("close_task_modal", _params, socket) do
    {:noreply, assign(socket, :task_modal, nil)}
  end

  # --- Board rename (local toggle; commit notifies the server) ----------------

  def handle_event("start_rename", _params, socket) do
    {:noreply, assign(socket, :renaming, true)}
  end

  def handle_event("cancel_rename", _params, socket) do
    {:noreply, assign(socket, :renaming, false)}
  end

  def handle_event("rename_board", %{"name" => name}, socket) do
    socket = assign(socket, :renaming, false)

    case String.trim(name) do
      "" ->
        {:noreply, socket}

      name ->
        # Optimistic: show the new name immediately; the host persists it and
        # re-pushes the authoritative name (rolling back if it was rejected).
        {:noreply,
         socket
         |> assign(:name, name)
         |> push_server_event("rename_board", %{"name" => name})}
    end
  end

  # --- Drag & drop (local until drop; commit notifies the server) ------------

  def handle_event("drag_start", %{"column_id" => cid, "task_id" => tid}, socket) do
    # get_in is nil-safe, so this validates that both the column and task exist.
    if get_in(socket.assigns.board, [cid, :tasks, tid]) do
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
        before_id = insertion_point(socket.assigns.board, dragging, cid, tid, params)
        target = resolve_target(socket.assigns.board, dragging, cid, before_id)
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
      target = resolve_target(socket.assigns.board, dragging, cid, nil)
      {:noreply, assign(socket, :drag_target, target)}
    else
      _ -> {:noreply, socket}
    end
  end

  def handle_event("drag_end", _params, socket) do
    case {socket.assigns.dragging, socket.assigns.drag_target} do
      {%{task_id: tid, source_column_id: src}, %{column_id: dst, before_task_id: before_id}} ->
        # Generate the new position locally (a rank between the destination
        # neighbors, with this task's id baked on) and move the card optimistically.
        position =
          socket.assigns.board[dst].tasks |> Map.values() |> Rank.key_before(before_id, tid)

        board = move_task(socket.assigns.board, src, tid, dst, position)

        socket =
          socket
          |> assign(board: board, dragging: nil, drag_target: nil)
          |> push_server_event("move_task", %{
            "task_id" => tid,
            "to_column_id" => dst,
            "position" => position
          })

        {:noreply, socket}

      _ ->
        {:noreply, assign(socket, dragging: nil, drag_target: nil)}
    end
  end

  # --- Drag helpers ----------------------------------------------------------

  # Where to insert when hovering `tid`: before the task when the cursor is in its
  # top half, after it otherwise.
  defp insertion_point(board, dragging, cid, tid, params) do
    offset_y = params["clientY"] - params["rect"]["top"]

    if offset_y < params["rect"]["height"] / 2 do
      tid
    else
      # Insert after the hovered task, skipping the dragged card when it sits
      # right there (it is about to leave this slot).
      dragged = dragging.task_id

      case successor_id(board, cid, tid) do
        ^dragged -> successor_id(board, cid, dragged)
        after_id -> after_id
      end
    end
  end

  # A same-column drop onto the card's own slot (its original successor, or the
  # end when it is already last) is a no-op, so drop the placeholder entirely.
  defp resolve_target(board, %{task_id: tid, source_column_id: src}, cid, before_id) do
    if cid == src and before_id == successor_id(board, src, tid) do
      nil
    else
      %{column_id: cid, before_task_id: before_id}
    end
  end

  defp move_task(board, src_id, task_id, dst_id, position) do
    task = %{get_in(board, [src_id, :tasks, task_id]) | position: position}

    {_task, board} = pop_in(board, [src_id, :tasks, task_id])
    put_in(board, [dst_id, :tasks, task_id], task)
  end

  # The id of the task following `tid` in `cid`'s on-screen (position) order, or
  # nil when it is last/absent.
  defp successor_id(board, cid, tid) do
    ids = board[cid].tasks |> sorted_tasks() |> Enum.map(& &1.id)

    case Enum.find_index(ids, &(&1 == tid)) do
      nil -> nil
      index -> Enum.at(ids, index + 1)
    end
  end

  # --- Positions -------------------------------------------------------------

  defp sorted_tasks(tasks), do: tasks |> Map.values() |> Enum.sort_by(& &1.position)

  # Append after the last column. max + 1 (not count) so it never collides with an
  # existing position once deletes leave gaps.
  defp next_column_position(board) do
    board
    |> Enum.map(fn {_id, %{position: pos}} -> pos + 1 end)
    |> Enum.max(fn -> 0 end)
  end

  defp uuid do
    <<a::48, _::4, b::12, _::2, c::62>> = :crypto.strong_rand_bytes(16)

    <<g1::binary-8, g2::binary-4, g3::binary-4, g4::binary-4, g5::binary-12>> =
      Base.encode16(<<a::48, 4::4, b::12, 2::2, c::62>>, case: :lower)

    "#{g1}-#{g2}-#{g3}-#{g4}-#{g5}"
  end

  @impl true
  def render(assigns) do
    assigns =
      assign(
        assigns,
        :board_sorted,
        assigns.board |> Map.values() |> Enum.sort_by(&{&1.position, &1.id})
      )

    ~H"""
    <div style={"font-family:sans-serif;color:#e5e7eb;padding:0.5em 0#{if @dragging, do: ";user-select:none"}"}>
      <BoardNameComponent.board_name name={@name} renaming={@renaming} />

      <div style="display:flex;gap:1em;overflow-x:auto;padding-bottom:1em;align-items:flex-start">
        <ColumnComponent.column
          :for={col <- @board_sorted}
          col={col}
          dragging={@dragging}
          drag_target={@drag_target}
        />

        <AddColumnComponent.add_column seq={@add_seq} />
      </div>

      <TaskModalComponent.modal params={@task_modal} />
    </div>
    """
  end
end
