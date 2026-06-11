defmodule BurritoWeb.InfoModal do
  use Phoenix.Component

  import BurritoWeb.CoreComponents

  def info_modal(assigns) do
    ~H"""
    <div
      id="info-modal"
      phx-update="ignore"
      onclick="if(event.target===this) window.closeInfoModal()"
      class="hidden fixed inset-0 z-50 flex items-center justify-center p-4 bg-black/60 backdrop-blur-sm"
    >
      <div class="info-modal-card relative w-full max-w-lg bg-pop-cream-warm border-2 border-pop-orange rounded-xl shadow-2xl p-6 flex flex-col gap-5">
        <button
          onclick="window.closeInfoModal()"
          class="absolute top-4 right-4 text-pop-brown-medium hover:text-pop-brown transition-colors cursor-pointer"
        >
          <.icon name="hero-x-mark" class="size-5" />
        </button>

        <div class="flex items-center gap-2">
          <.icon name="hero-cpu-chip" class="size-6 text-pop-orange shrink-0" />
          <h2 class="text-lg font-bold text-pop-brown">Local LiveView Demo</h2>
        </div>

        <div class="flex flex-col gap-3 text-sm text-pop-brown-medium leading-relaxed">
          <p>
            The same burrito form runs twice. Left as a standard Phoenix LiveView (server-rendered),
            right as <strong class="text-pop-brown">Local LiveView</strong>: Elixir code compiled
            to WASM and running directly in the browser.
          </p>

          <ul class="flex flex-col gap-2 list-none">
            <li class="flex items-start gap-2">
              <.icon name="hero-signal-slash" class="size-4 shrink-0 mt-0.5 text-pop-orange" />
              <span>
                <strong class="text-pop-brown">Disconnect</strong>
                - simulates going offline. The left freezes; the right keeps working. Reconnect to
                see the sync panel at the bottom update with what you did offline.
              </span>
            </li>
            <li class="flex items-start gap-2">
              <.icon name="hero-clock" class="size-4 shrink-0 mt-0.5 text-pop-orange" />
              <span>
                <strong class="text-pop-brown">Add 500ms Latency</strong>
                - the left becomes sluggish; the right stays instant.
              </span>
            </li>
          </ul>
        </div>

        <button
          onclick="window.closeInfoModal()"
          class="self-end px-4 py-2 text-sm font-semibold bg-pop-orange text-white rounded-lg hover:bg-pop-orange-dark transition-colors cursor-pointer"
        >
          Got it
        </button>
      </div>
    </div>
    """
  end
end
