defmodule BurritoWeb.OrderLive do
  use BurritoWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket) do
      Phoenix.PubSub.subscribe(Burrito.PubSub, "llv_mirror:BurritoLive")
    end

    {:ok, assign(socket, last_state: nil)}
  end

  def handle_info({:llv_attrs, attrs}, socket) do
    Phoenix.LiveView.send_update(BurritoWeb.Live.SyncTableComponent,
      id: "sync-table",
      sync_params: attrs
    )

    {:noreply, assign(socket, last_state: attrs)}
  end

  def render(assigns) do
    ~H"""
    <div class="h-[100dvh] md:h-screen flex flex-col overflow-auto md:overflow-hidden">
    <%!-- Demo control bar --%>
    <div class="flex-shrink-0 bg-pop-brown px-3 md:px-4 py-2 md:py-2.5 flex items-center gap-2 md:gap-3 flex-wrap shadow-md">
      <div class="flex items-center gap-1.5">
        <span id="connection-dot" class="inline-block w-2.5 h-2.5 rounded-full bg-green-400"></span>
        <span id="connection-label" class="text-xs md:text-sm font-medium text-white/80">Online</span>
      </div>

      <button
        id="elevator-btn"
        onclick="window.toggleElevator()"
        class="flex items-center gap-1.5 whitespace-nowrap px-2.5 md:px-3 py-1.5 text-[10px] sm:text-xs md:text-sm font-semibold rounded-md bg-pop-orange text-white hover:bg-pop-orange-dark transition-colors"
      >
        <.icon name="hero-signal-slash" class="size-4" /> Disconnect
      </button>

      <button
        id="latency-btn"
        onclick="window.toggleLatency()"
        class="flex items-center gap-1.5 px-2.5 md:px-3 py-1.5 text-xs md:text-sm font-medium rounded-md border border-white/20 text-white/80 hover:bg-white/10 transition-colors"
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
      <div class="w-1/2 min-w-0 px-3 md:px-6 py-2 md:py-3 bg-pop-grey border-r-2 border-pop-orange">
        <h2 class="inline-flex items-center gap-1.5 text-xs md:text-sm font-bold text-pop-brown tracking-wide uppercase leading-tight">
          <.icon name="hero-server" class="size-4 shrink-0" /> Standard LiveView
        </h2>
      </div>
      <div class="relative w-1/2 min-w-0 px-3 md:px-6 py-2 md:py-3 bg-pop-orange-light">
        <h2 class="inline-flex items-center gap-1.5 text-xs md:text-sm font-bold text-pop-orange-dark tracking-wide uppercase leading-tight">
          <.icon name="hero-cpu-chip" class="size-4 shrink-0" /> Local LiveView
        </h2>
      </div>
    </div>

    <%!-- Split screen --%>
    <div id="scroll-sync-root" phx-hook="ScrollSync" class="flex flex-1 min-h-0">

      <div class="w-1/2 min-w-0 border-r-2 border-pop-orange bg-pop-grey relative flex flex-col overflow-hidden">
        <div
          id="lv-unstable-strip"
          class="hidden absolute top-0 left-0 right-0 z-10 bg-pop-brown text-white text-[10px] sm:text-xs font-semibold text-center py-1 px-2 animate-pulse flex flex-row sm:flex-row items-center justify-center gap-0.5 sm:gap-1"
        >
          <.icon name="hero-exclamation-triangle" class="size-3.5" />
          <span class="sm:hidden">Disconnected</span>
          <span class="hidden sm:inline">Disconnected — no live connection to server</span>
        </div>
        <div id="left-scroll-panel" class="flex-1 min-h-0 overflow-auto">
          <.live_component module={BurritoWeb.Live.OrderingFormComponent} id="ordering-form" />
        </div>
      </div>

      <div class="w-1/2 min-w-0 bg-pop-orange-light relative flex flex-col overflow-hidden">
        <div
          id="llv-connection-strip"
          class="hidden absolute top-0 left-0 right-0 z-10 bg-pop-orange-dark text-white text-[10px] sm:text-xs font-semibold text-center py-1 px-2 flex items-center justify-center gap-1"
        >
          <.icon name="hero-signal-slash" class="size-3.5" />
          <span class="sm:hidden">Offline mode — syncing later</span>
          <span class="hidden sm:inline">No server connection — working offline, will sync on restore</span>
        </div>

        <div id="right-scroll-panel" class="border-b border-pop-orange/20 flex-1 min-h-0 overflow-auto">
          <.local_live_view view="BurritoLive" />
        </div>

      </div>
    </div>

      <div class="flex-shrink-0 overflow-auto max-h-72 md:max-h-97">
        <button
          id="sync-panel-toggle"
          onclick="window.toggleSyncedPanel()"
          class="w-full flex items-center justify-between px-3 py-2 text-xxs sm:text-sm font-medium transition-colors sticky top-0 z-10 bg-pop-brown hover:bg-pop-brown/90 border-b border-white/10"
        >
          <h4 class="text-xs sm:text-sm font-semibold text-white flex items-center gap-1.5">
            <.icon name="hero-arrow-path" class="size-4 text-pop-orange-light" /> Server received state
            <span class="text-xxs font-normal text-white/40">(synced from LocalLiveView)</span>
          </h4>
          <span id="sync-panel-toggle-state" class="flex items-center gap-1 text-xxs sm:text-xs text-white/50">
            Hide
            <.icon name="hero-chevron-up" class="size-3.5" />
          </span>
        </button>
        <div id="sync-panel-content" class="collapsible-grid open">
          <div>
            <.live_component module={BurritoWeb.Live.SyncTableComponent} id="sync-table" />
          </div>
        </div>
      </div>
    </div>
    """
  end
end
