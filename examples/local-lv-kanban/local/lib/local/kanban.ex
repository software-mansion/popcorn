defmodule Local.Kanban do
  use LocalLiveView

  alias Local.{AddColumnComponent, ColumnComponent, Rank, TaskModalComponent}

  # The board is a plain map `%{id => column}`; each column carries its tasks as
  # `%{id => task}`. Nothing tracks order explicitly — columns and tasks are
  # sorted by their `position` on render. Tasks use fractional-index string ranks
  # (`Rank`) with the task's id baked on (so positions are globally unique);
  # columns use the server's integer position. THE CLIENT generates task
  # positions: a drag/add computes the new rank locally and sends the literal
  # `position` to the host, which just persists it.
  #
  # This is the OPTIMISTIC, collaborative client: every edit is applied here
  # instantly AND sent to the host `BoardLive` via `push_server_event/3`, which
  # writes the DB and broadcasts so all viewers reconcile. The server is
  # authoritative: `update/2` rebuilds the whole board (preserving server ids)
  # whenever a new `rev` arrives, so a successful edit reconciles seamlessly and
  # a failed one rolls back. When a push never reaches the host at all
  # (disconnected socket), `handle_push_error/4` rolls the board back to the
  # last authoritative state.

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       name: nil,
       columns: %{},
       last_rev: nil,
       task_modal: nil,
       dragging: nil,
       drag_target: nil,
       add_seq: 0
     )}
  end

  @impl true
  def update(assigns, socket) do
    socket = assign(socket, :name, assigns[:name])

    # The host bumps `rev` on every authoritative push (incl. failure rollback,
    # where the board value is unchanged). Rebuild only when it changes.
    if assigns[:rev] == socket.assigns.last_rev do
      {:ok, socket}
    else
      {:ok,
       socket
       |> assign(:last_rev, assigns[:rev])
       |> assign(:columns, build_board(assigns.board))}
    end
  end

  @impl true
  def handle_push_error(_event, _params, server_assigns, socket) do
    # The default (feeding server_assigns through update/2) would no-op here:
    # the rolled-back rev already equals last_rev, so the guard skips the
    # rebuild. Force it, and drop any in-flight drag state that referenced the
    # rolled-back board.
    {:noreply,
     assign(socket,
       columns: build_board(server_assigns.board),
       dragging: nil,
       drag_target: nil
     )}
  end

  # --- Columns & tasks (optimistic) ------------------------------------------

  @impl true
  def handle_event("add_column", %{"name" => name}, socket) do
    case String.trim(name) do
      "" ->
        {:noreply, socket}

      name ->
        # The client owns the position: generate the id + an append position
        # (max + 1) and tell the host to persist them verbatim (reusing the same
        # id, so optimistic and authoritative columns converge). Bumping add_seq
        # re-mounts the (uncontrolled) add-column input so it clears — but only
        # after *this* client adds.
        id = uuid()
        position = next_column_position(socket.assigns.columns)
        column = %{id: id, name: name, position: position, tasks: %{}}

        {:noreply,
         socket
         |> assign(:columns, Map.put(socket.assigns.columns, id, column))
         |> assign(:add_seq, socket.assigns.add_seq + 1)
         |> push_server_event("add_column", %{
           "id" => id,
           "name" => name,
           "position" => position
         })}
    end
  end

  def handle_event("add_task", %{"column_id" => cid, "text" => text} = params, socket) do
    case {String.trim(text), socket.assigns.columns[cid]} do
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
          |> assign(:columns, put_in(socket.assigns.columns, [cid, :tasks, id], task))
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

  def handle_event("remove_column", %{"id" => id}, socket) do
    {_column, columns} = pop_in(socket.assigns.columns, [id])

    {:noreply,
     socket
     |> assign(:columns, columns)
     |> push_server_event("remove_column", %{"id" => id})}
  end

  def handle_event("remove_task", %{"column_id" => cid, "task_id" => tid}, socket) do
    {_task, columns} = pop_in(socket.assigns.columns, [cid, :tasks, tid])

    {:noreply,
     socket
     |> assign(:columns, columns)
     |> push_server_event("remove_task", %{"column_id" => cid, "task_id" => tid})}
  end

  # --- Task modal (local-only UI state) --------------------------------------

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

  # --- Drag & drop (local until drop; commit notifies the server) ------------

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
    case {socket.assigns.dragging, socket.assigns.drag_target} do
      {%{task_id: tid, source_column_id: src}, %{column_id: dst, before_task_id: before_id}} ->
        # Generate the new position locally (a rank between the destination
        # neighbors, with this task's id baked on) and move the card optimistically.
        position =
          socket.assigns.columns[dst].tasks |> Map.values() |> Rank.key_before(before_id, tid)

        columns = move_task(socket.assigns.columns, src, tid, dst, position)

        socket =
          socket
          |> assign(columns: columns, dragging: nil, drag_target: nil)
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

  defp move_task(columns, src_id, task_id, dst_id, position) do
    task = %{get_in(columns, [src_id, :tasks, task_id]) | position: position}

    {_task, columns} = pop_in(columns, [src_id, :tasks, task_id])
    put_in(columns, [dst_id, :tasks, task_id], task)
  end

  # The id of the task following `tid` in `cid`'s on-screen (position) order, or
  # nil when it is last/absent.
  defp successor_id(columns, cid, tid) do
    ids = columns[cid].tasks |> sorted_tasks() |> Enum.map(& &1.id)

    case Enum.find_index(ids, &(&1 == tid)) do
      nil -> nil
      index -> Enum.at(ids, index + 1)
    end
  end

  # --- Positions -------------------------------------------------------------

  defp sorted_tasks(tasks), do: tasks |> Map.values() |> Enum.sort_by(& &1.position)

  # Append after the last column. max + 1 (not count) so it never collides with an
  # existing position once deletes leave gaps.
  defp next_column_position(columns) do
    columns
    |> Enum.map(fn {_id, %{position: pos}} -> pos + 1 end)
    |> Enum.max(fn -> 0 end)
  end

  defp build_board(data) when is_list(data) do
    Map.new(data, fn entity ->
      entity = Map.new(entity, fn {k, v} -> {String.to_atom(k), build_board(v)} end)
      {entity.id, entity}
    end)
  end

  defp build_board(data) do
    data
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
        :columns_sorted,
        assigns.columns |> Map.values() |> Enum.sort_by(&{&1.position, &1.id})
      )

    ~H"""
    <div style={"font-family:sans-serif;color:#e5e7eb;padding:0.5em 0#{if @dragging, do: ";user-select:none"}"}>
      <h1 style="margin:0 0 0.75em;font-size:1.6em;font-weight:600;color:#f9fafb">{@name}</h1>

      <div style="display:flex;gap:1em;overflow-x:auto;padding-bottom:1em;align-items:flex-start">
        <ColumnComponent.column
          :for={col <- @columns_sorted}
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
