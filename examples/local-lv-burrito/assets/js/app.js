import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

const ScrollSync = {
  mounted() {
    const left = document.getElementById("left-scroll-panel");
    const right = document.getElementById("right-scroll-panel");
    if (!left || !right) return;

    const syncScroll = (from, to) => {
      const fromMax = from.scrollHeight - from.clientHeight;
      const toMax = to.scrollHeight - to.clientHeight;

      if (fromMax <= 0 || toMax <= 0) {
        to.scrollTop = 0;
        return;
      }

      const ratio = from.scrollTop / fromMax;
      to.scrollTop = ratio * toMax;
    };

    // activePanel is driven ONLY by explicit user-intent events (touch, wheel,
    // mouseenter, focus). Scroll handlers never touch it. This prevents the
    // feedback loop where a programmatic scrollTop write fires a scroll event
    // on the other panel, which flips activePanel and writes back — a write
    // that cancels iOS momentum scrolling and causes "ghost" scroll drift.
    let activePanel = right;
    let rafId = null;

    const syncFromActive = () => {
      if (activePanel === left) {
        syncScroll(left, right);
      } else {
        syncScroll(right, left);
      }
    };

    const scheduleSync = () => {
      if (rafId) return;
      rafId = requestAnimationFrame(() => {
        rafId = null;
        syncFromActive();
      });
    };

    const onLeftActive = () => {
      activePanel = left;
    };

    const onRightActive = () => {
      activePanel = right;
    };

    const onLeftScroll = () => {
      if (activePanel !== left) return;
      syncScroll(left, right);
    };

    const onRightScroll = () => {
      if (activePanel !== right) return;
      syncScroll(right, left);
    };

    // Observe both the scroll containers (so window resizes re-align) and
    // their inner content (so adding/removing items re-aligns the ratio).
    // We intentionally avoid MutationObserver here — it fires on attribute
    // churn from form inputs and during mobile touch-scroll, causing the
    // other panel to jump while the user is scrolling.
    const resizeObserver = new ResizeObserver(() => scheduleSync());
    resizeObserver.observe(left);
    resizeObserver.observe(right);
    if (left.firstElementChild) resizeObserver.observe(left.firstElementChild);
    if (right.firstElementChild)
      resizeObserver.observe(right.firstElementChild);

    left.addEventListener("scroll", onLeftScroll, { passive: true });
    right.addEventListener("scroll", onRightScroll, { passive: true });
    left.addEventListener("wheel", onLeftActive, { passive: true });
    right.addEventListener("wheel", onRightActive, { passive: true });
    left.addEventListener("touchstart", onLeftActive, { passive: true });
    right.addEventListener("touchstart", onRightActive, { passive: true });
    left.addEventListener("mouseenter", onLeftActive, { passive: true });
    right.addEventListener("mouseenter", onRightActive, { passive: true });
    left.addEventListener("focusin", onLeftActive);
    right.addEventListener("focusin", onRightActive);

    scheduleSync();

    this.cleanup = () => {
      if (rafId) cancelAnimationFrame(rafId);
      resizeObserver.disconnect();
      left.removeEventListener("scroll", onLeftScroll);
      right.removeEventListener("scroll", onRightScroll);
      left.removeEventListener("wheel", onLeftActive);
      right.removeEventListener("wheel", onRightActive);
      left.removeEventListener("touchstart", onLeftActive);
      right.removeEventListener("touchstart", onRightActive);
      left.removeEventListener("mouseenter", onLeftActive);
      right.removeEventListener("mouseenter", onRightActive);
      left.removeEventListener("focusin", onLeftActive);
      right.removeEventListener("focusin", onRightActive);
    };
  },

  destroyed() {
    this.cleanup && this.cleanup();
  },
};

let isOffline = false;
let latencyEnabled = false;

const MAX_RECONNECT_ATTEMPTS = 3;
const RECONNECT_TIMEOUT_MS = 5_000;
let reconnectAttempts = 0;
let reconnectWatchdog = null;
let reconnectDoneListener = null;

// Re-push every local LiveView's current state to the server mirror. Called
// after a /live reconnect so OrderLive (which just remounted) gets a fresh
// sync payload instead of waiting for the next user interaction.
function triggerLLVReconnect() {
  document
    .querySelectorAll("[data-pop-view][data-pop-mirror]")
    .forEach((el) => {
      window.__popcorn
        ?.call(
          { id: el.id, event: "llv_reconnected", payload: {} },
          { timeoutMs: 10_000 },
        )
        .catch((err) => console.error("LLV re-sync error", err));
    });
}

// Blocks clicks on the standard LiveView pane while the /live socket is down
// or mid-reconnect. Without this, a click during the reconnect gap reaches a
// view whose channel is still closed and LV logs "unmatched topic".
function setLeftPanelInteractive(interactive) {
  const el = document.getElementById("left-scroll-panel");
  if (!el) return;
  el.classList.toggle("pointer-events-none", !interactive);
  el.classList.toggle("opacity-60", !interactive);
}

function clearReconnectState() {
  if (reconnectWatchdog) {
    clearTimeout(reconnectWatchdog);
    reconnectWatchdog = null;
  }
  if (reconnectDoneListener) {
    window.removeEventListener("phx:page-loading-stop", reconnectDoneListener);
    reconnectDoneListener = null;
  }
}

function applyOnlineUI() {
  const btn = document.getElementById("elevator-btn");
  const dot = document.getElementById("connection-dot");
  const label = document.getElementById("connection-label");
  const lvStrip = document.getElementById("lv-unstable-strip");
  const llvStrip = document.getElementById("llv-connection-strip");

  lvStrip && lvStrip.classList.add("hidden");
  llvStrip && llvStrip.classList.add("hidden");
  dot && dot.classList.replace("bg-red-400", "bg-green-400");
  label && (label.textContent = "Online");
  btn &&
    (btn.innerHTML =
      '<span class="hero-signal-slash size-4"></span> Disconnect');
  btn && btn.classList.remove("bg-red-600", "hover:bg-red-700");
  btn && btn.classList.add("bg-pop-orange", "hover:bg-pop-orange-dark");
  btn && (btn.disabled = false);
  setLeftPanelInteractive(true);
}

function attemptReconnect() {
  reconnectAttempts += 1;
  window.liveSocket.connect();

  clearReconnectState();

  reconnectDoneListener = () => {
    reconnectDoneListener = null;
    clearReconnectState();
    reconnectAttempts = 0;
    applyOnlineUI();
    triggerLLVReconnect();
  };
  window.addEventListener("phx:page-loading-stop", reconnectDoneListener, {
    once: true,
  });

  reconnectWatchdog = setTimeout(() => {
    reconnectWatchdog = null;
    if (reconnectAttempts < MAX_RECONNECT_ATTEMPTS) {
      console.warn(
        `LV reconnect attempt ${reconnectAttempts} timed out, retrying`,
      );
      window.liveSocket.disconnect();
      attemptReconnect();
    } else {
      console.error(
        `LV reconnect failed after ${MAX_RECONNECT_ATTEMPTS} attempts — click Reconnect to try again`,
      );
      clearReconnectState();
      reconnectAttempts = 0;
      // Roll back to offline so the user can click Reconnect again.
      isOffline = true;
      const btn = document.getElementById("elevator-btn");
      btn && (btn.disabled = false);
      btn &&
        (btn.innerHTML = '<span class="hero-signal size-4"></span> Reconnect');
    }
  }, RECONNECT_TIMEOUT_MS);
}

window.toggleSyncedPanel = function () {
  const panel = document.getElementById("sync-panel-content");
  const state = document.getElementById("sync-panel-toggle-state");
  if (!panel || !state) return;

  const isOpen = panel.classList.contains("open");
  panel.classList.toggle("open", !isOpen);
  state.innerHTML = isOpen
    ? '<span>Show</span><span class="hero-chevron-down size-3.5"></span>'
    : '<span>Hide</span><span class="hero-chevron-up size-3.5"></span>';
};

window.toggleElevator = function () {
  isOffline = !isOffline;

  const btn = document.getElementById("elevator-btn");
  const dot = document.getElementById("connection-dot");
  const label = document.getElementById("connection-label");
  const lvStrip = document.getElementById("lv-unstable-strip");
  const llvStrip = document.getElementById("llv-connection-strip");

  if (isOffline) {
    clearReconnectState();
    reconnectAttempts = 0;
    window.liveSocket.disconnect();
    lvStrip && lvStrip.classList.remove("hidden");
    llvStrip && llvStrip.classList.remove("hidden");
    dot && dot.classList.replace("bg-green-400", "bg-red-400");
    label && (label.textContent = "Offline");
    btn &&
      (btn.innerHTML = '<span class="hero-signal size-4"></span> Reconnect');
    btn && btn.classList.add("bg-red-600", "hover:bg-red-700");
    btn && btn.classList.remove("bg-pop-orange", "hover:bg-pop-orange-dark");
    setLeftPanelInteractive(false);
  } else {
    // Keep the offline UI (strip visible, left panel blocked) until the LV
    // channel actually rejoins. Flipping to "Online" immediately would let
    // clicks fire into a half-connected LV and produce "unmatched topic"
    // errors during the reconnect gap.
    reconnectAttempts = 0;
    setLeftPanelInteractive(false);
    btn && (btn.disabled = true);
    btn &&
      (btn.innerHTML =
        '<span class="hero-arrow-path size-4 animate-spin"></span> Connecting…');
    attemptReconnect();
  }
};

window.toggleLatency = function () {
  latencyEnabled = !latencyEnabled;
  const btn = document.getElementById("latency-btn");
  if (latencyEnabled) {
    window.liveSocket.enableLatencySim(500);
    btn && btn.classList.add("bg-white/20", "border-white/40");
    btn && btn.classList.remove("border-white/20");
    btn &&
      (btn.innerHTML =
        '<span class="hero-clock size-4"></span> Latency ON (500ms)');
  } else {
    window.liveSocket.disableLatencySim();
    btn && btn.classList.remove("bg-white/20", "border-white/40");
    btn && btn.classList.add("border-white/20");
    btn &&
      (btn.innerHTML =
        '<span class="hero-clock size-4"></span> Add 500ms Latency');
  }
};

// ─── LiveSocket Setup ─────────────────────────────────────────────────────────

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: { ScrollSync },
});

topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(300));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

// Setup Local LiveViews (intercepts LLV data-pop-view elements and runs them via WASM)
import { setup } from "local_live_view";

liveSocket.connect();
window.liveSocket = liveSocket;

setup(liveSocket, { Socket, bundlePaths: ["bundle.avm"] });

if (process.env.NODE_ENV === "development") {
  window.addEventListener(
    "phx:live_reload:attached",
    ({ detail: reloader }) => {
      reloader.enableServerLogs();
      let keyDown;
      window.addEventListener("keydown", (e) => (keyDown = e.key));
      window.addEventListener("keyup", () => (keyDown = null));
      window.addEventListener(
        "click",
        (e) => {
          if (keyDown === "c") {
            e.preventDefault();
            e.stopImmediatePropagation();
            reloader.openEditorAtCaller(e.target);
          } else if (keyDown === "d") {
            e.preventDefault();
            e.stopImmediatePropagation();
            reloader.openEditorAtDef(e.target);
          }
        },
        true,
      );
      window.liveReloader = reloader;
    },
  );
}
