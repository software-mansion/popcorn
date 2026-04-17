defmodule PlayLocal do
  use LocalLiveView

  @grid_cols 25
  @grid_rows 15

  defp cell_key(x, y), do: x + y * @grid_cols
  defp key_to_xy(k), do: {rem(k, @grid_cols), div(k, @grid_cols)}

  # ── Lifecycle ────────────────────────────────────────────────────────────────

  def mount(_params, _session, socket) do
    {:ok,
     socket
     |> assign(:cells, %{})
     |> assign(:my_keys, [])
     |> assign(:my_keys_set, MapSet.new())
     # Durable local record of cells owned by this user — survives initialize resets
     |> assign(:my_cells, %{})
     |> assign(:owner_id, "pending")
     |> assign(:nick, nil)
     |> assign(:show_nick_prompt, true)
     |> assign(:grid_cols, @grid_cols)
     |> assign(:grid_rows, @grid_rows)}
  end

  # ── Server → Local events ────────────────────────────────────────────────────

  def handle_server_event("initialize", payload, socket) do
    cells_list = payload["cells"] || []
    owner_id = payload["owner_id"] || socket.assigns.owner_id

    {cells, my_keys, my_keys_set} =
      Enum.reduce(cells_list, {%{}, [], MapSet.new()}, fn cell, {cells, my_keys, my_keys_set} ->
        k = cell_key(cell["x"], cell["y"])

        cell_data = %{
          nick: cell["nick"],
          owner_id: cell["owner_id"],
          timestamp: cell["timestamp"] || 0,
          claimed_offline: cell["claimed_offline"] || false
        }

        {my_keys, my_keys_set} =
          if cell["owner_id"] == owner_id,
            do: {[k | my_keys], MapSet.put(my_keys_set, k)},
            else: {my_keys, my_keys_set}

        {Map.put(cells, k, cell_data), my_keys, my_keys_set}
      end)

    # Overlay locally-owned cells that the server doesn't know about yet.
    # This keeps the user's cells visible immediately after server restart;
    # the restore_request flow will push them back to the server shortly after.
    {cells, my_keys, my_keys_set} =
      Enum.reduce(socket.assigns.my_cells, {cells, my_keys, my_keys_set}, fn {k, cell},
                                                                             {cells, my_keys,
                                                                              my_keys_set} ->
        if Map.has_key?(cells, k) do
          # Server already has this cell (another restore beat us, or it survived restart)
          {cells, my_keys, my_keys_set}
        else
          {Map.put(cells, k, cell), [k | my_keys], MapSet.put(my_keys_set, k)}
        end
      end)

    {:noreply,
     socket
     |> assign(:owner_id, owner_id)
     |> assign(:cells, cells)
     |> assign(:my_keys, my_keys)
     |> assign(:my_keys_set, my_keys_set)}
  end

  def handle_server_event("cell_update", payload, socket) do
    k = cell_key(payload["x"], payload["y"])

    cell = %{
      nick: payload["nick"],
      owner_id: payload["owner_id"],
      timestamp: payload["timestamp"] || 0,
      claimed_offline: payload["claimed_offline"] || false
    }

    is_now_mine = cell.owner_id == socket.assigns.owner_id

    # If someone else claimed a cell we thought was ours, evict it everywhere
    my_keys =
      if is_now_mine,
        do: socket.assigns.my_keys,
        else: List.delete(socket.assigns.my_keys, k)

    my_keys_set =
      if is_now_mine,
        do: socket.assigns.my_keys_set,
        else: MapSet.delete(socket.assigns.my_keys_set, k)

    my_cells =
      if is_now_mine,
        do: Map.put(socket.assigns.my_cells, k, cell),
        else: Map.delete(socket.assigns.my_cells, k)

    {:noreply,
     socket
     |> assign(:cells, Map.put(socket.assigns.cells, k, cell))
     |> assign(:my_keys, my_keys)
     |> assign(:my_keys_set, my_keys_set)
     |> assign(:my_cells, my_cells)}
  end

  def handle_server_event("cell_released", payload, socket) do
    k = cell_key(payload["x"], payload["y"])

    {:noreply,
     socket
     |> assign(:cells, Map.delete(socket.assigns.cells, k))
     |> assign(:my_keys, List.delete(socket.assigns.my_keys, k))
     |> assign(:my_keys_set, MapSet.delete(socket.assigns.my_keys_set, k))
     |> assign(:my_cells, Map.delete(socket.assigns.my_cells, k))}
  end

  def handle_server_event("grid_reset", _payload, socket) do
    {:noreply,
     socket
     |> assign(:cells, %{})
     |> assign(:my_keys, [])
     |> assign(:my_keys_set, MapSet.new())
     |> assign(:my_cells, %{})}
  end

  def handle_server_event(_event, _payload, socket) do
    {:noreply, socket}
  end

  def handle_info(_msg, socket), do: {:noreply, socket}

  # ── User events ──────────────────────────────────────────────────────────────

  def handle_event("set_nick", %{"nick" => nick}, socket) do
    nick = String.trim(nick)

    if String.length(nick) > 0 do
      {:noreply, socket |> assign(:nick, nick) |> assign(:show_nick_prompt, false)}
    else
      {:noreply, socket}
    end
  end

  # Single event for both claim and release — keeps phx-click static in template
  def handle_event("tap_cell", %{"k" => k_str}, socket) do
    k = String.to_integer(k_str)
    {x, y} = key_to_xy(k)
    nick = socket.assigns.nick

    if nick == nil do
      {:noreply, assign(socket, :show_nick_prompt, true)}
    else
      owner_id = socket.assigns.owner_id

      if MapSet.member?(socket.assigns.my_keys_set, k) do
        socket =
          socket
          |> assign(:my_keys, List.delete(socket.assigns.my_keys, k))
          |> assign(:my_keys_set, MapSet.delete(socket.assigns.my_keys_set, k))
          |> assign(:my_cells, Map.delete(socket.assigns.my_cells, k))
          |> assign(:cells, Map.delete(socket.assigns.cells, k))

        LocalLiveView.mirror_sync(socket, [:my_cells])
        {:noreply, socket}
      else
        ts = :os.system_time(:millisecond)
        cell = %{nick: nick, owner_id: owner_id, timestamp: ts, claimed_offline: false}

        socket =
          socket
          |> assign(:my_keys, [k | socket.assigns.my_keys])
          |> assign(:my_keys_set, MapSet.put(socket.assigns.my_keys_set, k))
          |> assign(:my_cells, Map.put(socket.assigns.my_cells, k, cell))
          |> assign(:cells, Map.put(socket.assigns.cells, k, cell))

        LocalLiveView.mirror_sync(socket, [:my_cells])
        {:noreply, socket}
      end
    end
  end

  def handle_event("show_nick_prompt", _params, socket) do
    {:noreply, assign(socket, :show_nick_prompt, true)}
  end

  def handle_event("dismiss_nick_prompt", _params, socket) do
    if socket.assigns.nick != nil do
      {:noreply, assign(socket, :show_nick_prompt, false)}
    else
      {:noreply, socket}
    end
  end

  # ── Render ───────────────────────────────────────────────────────────────────

  def render(assigns) do
    ~H"""
    <div id="play-root" class="min-h-screen bg-light-20 font-inter flex items-center flex-col">
      <%= if @show_nick_prompt do %>
        <div class="fixed inset-0 bg-[rgba(48,27,5,0.6)] flex items-center justify-center z-[100]">
          <div class="bg-white rounded-2xl p-8 px-7 max-w-[340px] w-[90%] shadow-[0_20px_60px_rgba(0,0,0,0.3)]">
            <h2 class="font-handjet text-4xl text-brown-header mt-0 mb-5 tracking-[0.05em] font-semibold">Your nick</h2>
            <form phx-submit="set_nick" class="flex flex-col gap-3">
              <input
                type="text"
                name="nick"
                value=""
                placeholder="nickname123"
                autofocus
                autocomplete="off"
                maxlength="12"
                class="text-xl px-4 py-3 border-2 border-grey-20 rounded-[10px] outline-none w-full box-border font-handjet text-brown-100 bg-light-30"
              />
              <button type="submit" class="bg-orange-100 text-white border-0 rounded-[10px] py-3.5 font-semibold cursor-pointer font-handjet text-xl">Play!</button>
            </form>
            <%= if @nick != nil do %>
              <button phx-click="dismiss_nick_prompt" class="mt-3 w-full bg-transparent border-0 text-brown-gray text-sm cursor-pointer py-1">Anuluj</button>
            <% end %>
          </div>
        </div>
      <% end %>

      <div class="w-full">
        <div class="px-4 py-3 flex items-center justify-between border-b border-grey-20">
          <span class="font-handjet text-2xl text-brown-header tracking-[0.05em]">The Immortal Grid</span>
          <div class="flex items-center gap-3">
            <%= if @nick != nil do %>
              <button phx-click="show_nick_prompt" class="bg-orange-20 text-brown-header border-0 rounded-full px-3 py-1 text-sm font-semibold cursor-pointer font-handjet">{@nick}</button>
            <% end %>
            <div id="play-status-dot" class="w-2.5 h-2.5 rounded-full bg-[#22c55e] shadow-[0_0_6px_#22c55e]"></div>
          </div>
        </div>

        <div id="offline-banner" class="bg-yellow-50 border-b border-yellow-200 px-4 py-2 text-[0.8rem] text-yellow-700 text-center" style="display: none;">
          Offline — zmiany zsynchronizuja sie po powrocie serwera
        </div>

        <%= if @nick == nil do %>
          <div class="p-4 text-center text-brown-gray text-sm cursor-pointer" phx-click="show_nick_prompt">
            Tap to set your nick and claim cells
          </div>
        <% end %>
      </div>

      <div class="p-3 w-full h-full flex items-center justify-center overflow-x-auto flex-1">
        <div class="relative">
          <%!-- Layer 1: click targets — 1 dynamic attr each (phx-value-k), always 200 divs --%>
          <div class="grid gap-[3px]" style={"grid-template-columns: repeat(#{@grid_cols}, 44px);"}>
            <%= for k <- 0..(@grid_cols * @grid_rows - 1) do %>
              <div
                phx-click="tap_cell"
                phx-value-k={k}
                class="size-11 cursor-pointer bg-grey-10 border border-grey-20 rounded-[3px]"
              ></div>
            <% end %>
          </div>
          <%!-- Layer 2: claimed cell overlays — iterates only @cells (sparse) --%>
          <div
            class="absolute inset-0 grid gap-[3px] pointer-events-none"
            style={"grid-template-rows: repeat(#{@grid_rows}, 44px); grid-template-columns: repeat(#{@grid_cols}, 44px);"}
          >
            <%= for {k, cell} <- @cells do %>
              <% is_mine = MapSet.member?(@my_keys_set, k) %>
              <% {cx, cy} = key_to_xy(k) %>
              <div
                id={"o#{k}"}
                phx-click="tap_cell"
                phx-value-k={k}
                class={cell_class(cell, is_mine)}
                style={"grid-column: #{cx + 1}; grid-row: #{cy + 1};"}
              >{cell_nick(cell)}</div>
            <% end %>
          </div>
        </div>
      </div>

      <%= if length(@my_keys) > 0 do %>
        <div class="px-4 py-2 text-[0.8rem] text-brown-70 text-center">
          You own <strong>{length(@my_keys)}</strong> cells
        </div>
      <% end %>
    </div>
    """
  end

  @gc_base "size-11 flex items-center justify-center rounded-[3px] cursor-pointer pointer-events-auto " <>
             "font-handjet text-[9px] font-bold text-center leading-[1.1] " <>
             "break-all overflow-hidden p-0.5 box-border"

  defp cell_class(%{claimed_offline: true}, true),
    do: @gc_base <> " bg-yellow-600 border-2 border-yellow-700 text-white"

  defp cell_class(%{claimed_offline: true}, false),
    do: @gc_base <> " bg-yellow-600 border border-yellow-700 text-light-20"

  defp cell_class(_, true),
    do: @gc_base <> " bg-orange-100 border-2 border-orange-200 text-white"

  defp cell_class(_, false),
    do: @gc_base <> " bg-orange-20 border border-orange-100 text-brown-header"

  defp cell_nick(nil), do: ""
  defp cell_nick(%{nick: nil}), do: ""
  defp cell_nick(%{nick: nick}), do: nick
end
