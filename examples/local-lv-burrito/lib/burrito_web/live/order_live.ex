defmodule BurritoWeb.OrderLive do
  use BurritoWeb, :live_view

  def mount(_params, _session, socket) do
    {:ok, assign(socket, last_state: nil, open_synced: true, open_log: false)}
  end

  def handle_event("toggle_synced", _params, socket) do
    {:noreply, assign(socket, open_synced: !socket.assigns.open_synced)}
  end

  def handle_event("toggle_log", _params, socket) do
    {:noreply, assign(socket, open_log: !socket.assigns.open_log)}
  end

  def handle_event("state_update", params, socket) do
    Phoenix.LiveView.send_update(BurritoWeb.Live.SyncTableComponent,
      id: "sync-table",
      sync_params: params
    )

    {:reply, %{message: "ok"}, assign(socket, last_state: params)}
  end

  def handle_event("sync_request", _params, socket) do
    socket =
      case socket.assigns.last_state do
        nil ->
          socket

        state ->
          push_to_local(socket, "BurritoLive", %{"type" => "synchronize", "state" => state})
      end

    {:noreply, socket}
  end

  # LLV events (set_base, set_protein, etc.) can leak through to OrderLive when the
  # local view is destroyed during a reconnect cycle and Phoenix falls back to routing
  # through the parent channel. Ignore them — Popcorn handles them once the LLV view
  # recovers.
  def handle_event(_event, _params, socket) do
    {:noreply, socket}
  end

  def render(assigns) do
    ~H"""
    <div class="h-screen flex flex-col overflow-hidden">
    <%!-- Demo control bar --%>
    <div class="flex-shrink-0 bg-pop-brown px-4 py-2.5 flex items-center gap-3 flex-wrap shadow-md">
      <div class="flex items-center gap-1.5">
        <span id="connection-dot" class="inline-block w-2.5 h-2.5 rounded-full bg-green-400"></span>
        <span id="connection-label" class="text-sm font-medium text-white/80">Online</span>
      </div>

      <button
        id="elevator-btn"
        onclick="window.toggleElevator()"
        class="flex items-center gap-1.5 px-3 py-1.5 text-sm font-semibold rounded-md bg-pop-orange text-white hover:bg-pop-orange-dark transition-colors"
      >
        <.icon name="hero-signal-slash" class="size-4" /> Disconnect
      </button>

      <button
        id="latency-btn"
        onclick="window.toggleLatency()"
        class="flex items-center gap-1.5 px-3 py-1.5 text-sm font-medium rounded-md border border-white/20 text-white/80 hover:bg-white/10 transition-colors"
      >
        <.icon name="hero-clock" class="size-4" /> Add 500ms Latency
      </button>

      <div
        id="sync-toast"
        class="hidden ml-auto flex items-center gap-1 text-xs font-medium text-green-300 bg-green-900/50 border border-green-700/50 px-3 py-1 rounded-full"
      >
        <.icon name="hero-check-circle" class="size-3.5" /> Synced!
      </div>
    </div>

    <%!-- Column headers --%>
    <div class="flex flex-shrink-0">
      <div class="w-1/2 px-6 py-3 bg-pop-grey border-r-2 border-pop-orange">
        <h2 class="flex items-center gap-1.5 text-sm font-bold text-pop-brown tracking-wide uppercase">
          <.icon name="hero-server" class="size-4" /> Standard LiveView
          <span class="ml-1 text-xs font-normal normal-case text-pop-brown-medium/60">
            Server round-trips
          </span>
        </h2>
      </div>
      <div class="w-1/2 px-6 py-3 bg-pop-orange-light flex items-center justify-between">
        <h2 class="flex items-center gap-1.5 text-sm font-bold text-pop-orange-dark tracking-wide uppercase">
          <.icon name="hero-cpu-chip" class="size-4" /> Local LiveView
          <span class="ml-1 text-xs font-normal normal-case text-pop-orange/80">
            Instant, in browser
          </span>
        </h2>
        <span
          id="llv-sync-status"
          class="flex items-center gap-1 text-xs font-semibold px-2 py-0.5 rounded-full bg-green-100 text-green-700"
        >
          <.icon name="hero-check-circle" class="size-3.5" /> Synced
        </span>
      </div>
    </div>

    <%!-- Split screen --%>
    <div class="flex flex-1 min-h-0">
      <div class="w-1/2 border-r-2 border-pop-orange bg-pop-grey relative overflow-auto">
        <div
          id="lv-unstable-strip"
          class="hidden absolute top-0 left-0 right-0 z-10 bg-pop-brown text-white text-xs font-semibold text-center py-1 animate-pulse flex items-center justify-center gap-1"
        >
          <.icon name="hero-exclamation-triangle" class="size-3.5" />
          Disconnected — no live connection to server
        </div>
        <.live_component module={BurritoWeb.Live.OrderingFormComponent} id="ordering-form" />
      </div>

      <div class="w-1/2 bg-pop-orange-light relative flex flex-col overflow-hidden">
        <div
          id="llv-connection-strip"
          class="hidden absolute top-0 left-0 right-0 z-10 bg-pop-orange-dark text-white text-xs font-semibold text-center py-1 flex items-center justify-center gap-1"
        >
          <.icon name="hero-signal-slash" class="size-3.5" />
          No server connection — working offline, will sync on restore
        </div>

        <div class="border-b border-pop-orange/20 flex-1 min-h-0 overflow-auto">
          <.local_live_view view="BurritoLive" />
        </div>

        <%!-- Collapsible "server received state" panel --%>
        <div class="overflow-auto max-h-110">
          <button
            class="w-full flex items-center justify-between px-3 py-2 text-sm font-medium transition-colors sticky top-0 z-10 bg-pop-brown hover:bg-pop-brown/90 border-b border-white/10"
            phx-click="toggle_synced"
          >
            <h4 class="text-sm font-semibold text-white flex items-center gap-1.5">
              <.icon name="hero-arrow-path" class="size-4 text-pop-orange-light" /> Server received state
              <span class="text-xs font-normal text-white/40">(synced from LocalLiveView)</span>
            </h4>
            <span class="flex items-center gap-1 text-xs text-white/50">
              {if @open_synced, do: "Hide", else: "Show"}
              <.icon
                name={if @open_synced, do: "hero-chevron-up", else: "hero-chevron-down"}
                class="size-3.5"
              />
            </span>
          </button>
          <div class={"collapsible-grid #{if @open_synced, do: "open", else: ""}"}>
            <div>
              <.live_component module={BurritoWeb.Live.SyncTableComponent} id="sync-table" />
            </div>
          </div>
        </div>
      </div>
    </div>

    <%!-- Event log --%>
    <div class="flex-shrink-0 bg-pop-brown">
      <button
        phx-click="toggle_log"
        class="w-full flex items-center justify-between px-4 py-2.5 hover:bg-white/5 transition-colors"
      >
        <h3 class="flex items-center gap-2 text-xs font-semibold text-white/60 uppercase tracking-widest">
          <.icon name="hero-queue-list" class="size-3.5" /> Event Log
        </h3>
        <div class="flex items-center gap-3">
          <span class="flex items-center gap-1.5 text-xs text-white/30">
            <span class="inline-block w-2 h-2 rounded-sm bg-blue-400/70"></span> LV
            <span class="inline-block w-2 h-2 rounded-sm bg-pop-orange ml-1"></span> LLV
            <span class="inline-block w-2 h-2 rounded-sm bg-white/20 ml-1"></span> sys
          </span>
          <span class="flex items-center gap-1 text-xs text-white/30">
            {if @open_log, do: "Hide", else: "Show"}
            <.icon
              name={if @open_log, do: "hero-chevron-down", else: "hero-chevron-up"}
              class="size-3.5"
            />
          </span>
        </div>
      </button>
      <div class={"collapsible-grid #{if @open_log, do: "open", else: ""}"}>
        <div class="border-t border-white/10">
          <div
            id="event-log-entries"
            phx-update="ignore"
            class="h-44 overflow-y-auto px-4 py-2 space-y-px font-mono text-xs"
          >
          </div>
          <div class="flex justify-end px-4 py-1.5 border-t border-white/10">
            <button
              onclick="document.getElementById('event-log-entries').innerHTML = ''"
              class="text-xs text-white/30 hover:text-white/70 border border-white/10 hover:border-white/30 px-2 py-0.5 rounded transition-colors"
            >
              Clear
            </button>
          </div>
        </div>
      </div>
    </div>
    </div>
    """
  end
end
