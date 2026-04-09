import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

// ─── Event Log ───────────────────────────────────────────────────────────────

function logEvent(source, message) {
  const log = document.getElementById("event-log-entries");
  if (!log) return;

  const now = new Date();
  const ts =
    [
      now.getHours().toString().padStart(2, "0"),
      now.getMinutes().toString().padStart(2, "0"),
      now.getSeconds().toString().padStart(2, "0"),
    ].join(":") +
    "." +
    now.getMilliseconds().toString().padStart(3, "0");

  const entry = document.createElement("div");
  entry.className =
    "log-entry flex items-baseline gap-2.5 py-1 border-b border-white/5 last:border-0";

  const tsEl = document.createElement("span");
  tsEl.className = "text-white/25 flex-shrink-0 tabular-nums";
  tsEl.textContent = ts;

  const tagEl = document.createElement("span");
  tagEl.className =
    "flex-shrink-0 px-1.5 py-px rounded text-xs font-bold leading-none";
  if (source === "lv") {
    tagEl.className += " bg-blue-900/60 text-blue-300";
    tagEl.textContent = "LV";
  } else if (source === "llv") {
    tagEl.className += " bg-orange-900/50 text-orange-300";
    tagEl.textContent = "LLV";
  } else {
    tagEl.className += " bg-white/10 text-white/40";
    tagEl.textContent = "sys";
  }

  const msgEl = document.createElement("span");
  msgEl.className = "text-white/70 leading-relaxed";
  msgEl.textContent = message;

  entry.appendChild(tsEl);
  entry.appendChild(tagEl);
  entry.appendChild(msgEl);
  log.appendChild(entry);
  log.scrollTop = log.scrollHeight;
}

window.logEvent = logEvent;

// ─── Elevator & Latency Controls ──────────────────────────────────────────────

let isOffline = false;
let latencyEnabled = false;

function setLLVSyncStatus(state, count = 0) {
  const el = document.getElementById("llv-sync-status");
  if (!el) return;
  const configs = {
    synced: {
      text: "Synced",
      icon: "hero-check-circle",
      cls: "bg-green-100 text-green-700",
    },
    pending: {
      text: count > 0 ? `${count} pending` : "Pending",
      icon: "hero-clock",
      cls: "bg-yellow-100 text-yellow-700",
    },
    syncing: {
      text: "Syncing…",
      icon: "hero-arrow-path animate-spin",
      cls: "bg-blue-100 text-blue-700",
    },
    failed: {
      text: "Sync failed",
      icon: "hero-x-circle",
      cls: "bg-red-100 text-red-700",
    },
  };
  const cfg = configs[state];
  el.className = `flex items-center gap-1 text-xs font-semibold px-2 py-0.5 rounded-full ${cfg.cls}`;
  el.innerHTML = `<span class="${cfg.icon} size-3.5"></span> ${cfg.text}`;
}

window.toggleElevator = function () {
  isOffline = !isOffline;

  const btn = document.getElementById("elevator-btn");
  const dot = document.getElementById("connection-dot");
  const label = document.getElementById("connection-label");
  const lvStrip = document.getElementById("lv-unstable-strip");
  const llvStrip = document.getElementById("llv-connection-strip");

  if (isOffline) {
    window.liveSocket.disconnect();
    lvEventName = null;
    lvEventStart = null;
    lvStrip && lvStrip.classList.remove("hidden");
    llvStrip && llvStrip.classList.remove("hidden");
    dot && dot.classList.replace("bg-green-400", "bg-red-400");
    label && (label.textContent = "Offline");
    btn &&
      (btn.innerHTML =
        '<span class="hero-signal size-4"></span> Restore Connection');
    btn && btn.classList.add("bg-red-600", "hover:bg-red-700");
    btn && btn.classList.remove("bg-pop-orange", "hover:bg-pop-orange-dark");
    logEvent("system", "DISCONNECTED");
    logEvent("lv", "no server — UI frozen");
    logEvent("llv", "no server — operating locally, changes queued");
  } else {
    window.liveSocket.connect();
    lvStrip && lvStrip.classList.add("hidden");
    llvStrip && llvStrip.classList.add("hidden");
    dot && dot.classList.replace("bg-red-400", "bg-green-400");
    label && (label.textContent = "Online");
    btn &&
      (btn.innerHTML =
        '<span class="hero-signal-slash size-4"></span> Disconnect');
    btn && btn.classList.remove("bg-red-600", "hover:bg-red-700");
    btn && btn.classList.add("bg-pop-orange", "hover:bg-pop-orange-dark");
    logEvent("system", "CONNECTION RESTORED");
    logEvent("lv", "reconnecting... full state reload");
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
    logEvent("system", "Latency enabled (500ms on LV side)");
  } else {
    window.liveSocket.disableLatencySim();
    btn && btn.classList.remove("bg-white/20", "border-white/40");
    btn && btn.classList.add("border-white/20");
    btn &&
      (btn.innerHTML =
        '<span class="hero-clock size-4"></span> Add 500ms Latency');
    logEvent("system", "Latency disabled");
  }
};

// ─── LLV State Tracking ────────────────────────────────────────────────────────

let previousLLVState = null;

function logLLVStateTransitions(newState) {
  if (!previousLLVState) {
    previousLLVState = newState;
    return;
  }
  const b = newState.builder;
  const p = previousLLVState.builder;
  for (const f of ["base", "protein", "quantity", "notes"]) {
    if (b[f] !== p[f]) logEvent("llv", `${f}: ${p[f]} → ${b[f]}`);
  }
  b.toppings
    .filter((t) => !p.toppings.includes(t))
    .forEach((t) => logEvent("llv", `topping added: ${t}`));
  p.toppings
    .filter((t) => !b.toppings.includes(t))
    .forEach((t) => logEvent("llv", `topping removed: ${t}`));
  b.extras
    .filter((e) => !p.extras.includes(e))
    .forEach((e) => logEvent("llv", `extra added: ${e}`));
  p.extras
    .filter((e) => !b.extras.includes(e))
    .forEach((e) => logEvent("llv", `extra removed: ${e}`));
  previousLLVState = newState;
}

// ─── LLV State Events (via ServerSendHook WebSocket bridge) ───────────────────

document.addEventListener("DOMContentLoaded", () => {
  const llvEl = document.getElementById("BurritoLive");
  if (!llvEl) return;

  llvEl.addEventListener("serverSend", (e) => {
    if (isOffline) {
      logEvent("llv", `${e.detail.event_name} queued — offline`);
    } else {
      logEvent("llv", `${e.detail.event_name} → server`);
    }
  });

  llvEl.addEventListener("llv-sync-ack", () => {
    setLLVSyncStatus("synced");
    logEvent("llv", "✅ acknowledged by server");
  });
});

// ─── LV Round-Trip Timing ─────────────────────────────────────────────────────

let lvEventName = null;
let lvEventStart = null;

document.addEventListener(
  "click",
  (e) => {
    const lvContainer = document.querySelector(".lv-order-builder");
    const target = e.target.closest("[phx-click]");
    if (target && lvContainer && lvContainer.contains(target)) {
      lvEventName = target.getAttribute("phx-click");
      lvEventStart = Date.now();
      logEvent("lv", `${lvEventName} sent → waiting...`);
    }
  },
  true,
);

// ─── LiveSocket Setup ─────────────────────────────────────────────────────────

const Hooks = {};

Hooks.LVRoundTripHook = {
  updated() {
    if (lvEventStart !== null) {
      const ms = Date.now() - lvEventStart;
      logEvent("lv", `${lvEventName || "event"} acknowledged (${ms}ms)`);
      lvEventName = null;
      lvEventStart = null;
    }
  },
};

Hooks.ServerSendHook = {
  mounted() {
    this._queue = [];
    this._connected = true;

    this.el.addEventListener("serverSend", (e) => {
      if (!this._connected || !window.liveSocket.isConnected()) {
        this._queue = this._queue.filter(
          (item) => item.event_name !== e.detail.event_name,
        );
        this._queue.push({
          event_name: e.detail.event_name,
          payload: e.detail.payload,
        });
        setLLVSyncStatus("pending", this._queue.length);
        return;
      }

      this.pushEvent(e.detail.event_name, e.detail.payload, () => {
        this.el.dispatchEvent(
          new CustomEvent("llv-sync-ack", { bubbles: true }),
        );
      });
    });
  },

  disconnected() {
    this._connected = false;
  },

  reconnected() {
    this._connected = true;
    if (this._queue.length === 0) return;
    const queue = this._queue;
    this._queue = [];
    setLLVSyncStatus("syncing");

    let remaining = queue.length;
    queue.forEach(({ event_name, payload }) => {
      this.pushEvent(event_name, payload, () => {
        if (--remaining === 0) {
          setLLVSyncStatus("synced");
          logEvent(
            "llv",
            `✅ ${queue.length} queued event(s) flushed after reconnect`,
          );
          this.el.dispatchEvent(
            new CustomEvent("llv-sync-ack", { bubbles: true }),
          );
        }
      });
    });
  },
};

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});

topbar.config({ barColors: { 0: "#29d" }, shadowColor: "rgba(0, 0, 0, .3)" });
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(300));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

// Setup Local LiveViews (intercepts LLV data-pop-view elements and runs them via WASM)
import { setup } from "local_live_view";
setup(liveSocket, { bundlePath: "bundle.avm" });

liveSocket.connect();
window.liveSocket = liveSocket;

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
