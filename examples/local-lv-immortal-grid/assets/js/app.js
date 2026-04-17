import "phoenix_html";
import { Socket } from "phoenix";
import { LiveSocket } from "phoenix_live_view";
import topbar from "../vendor/topbar";

// ─── Connection state tracking ────────────────────────────────────────────────

function setPlayOfflineBanner(offline) {
  const banner = document.getElementById("offline-banner");
  const dot = document.getElementById("play-status-dot");
  if (banner) banner.style.display = offline ? "block" : "none";
  if (dot) {
    dot.style.background = offline ? "#cf3f3f" : "#22c55e";
    dot.title = offline ? "offline" : "connected";
  }
}

// ─── Hooks ────────────────────────────────────────────────────────────────────

const Hooks = {};

// ─── Presenter: phx-disconnected CSS for grid fade + overlay ──────────────────
//
// Phoenix LiveView adds class `phx-disconnected` to the HTML root element
// when the server connection is lost. We use MutationObserver to watch for
// this and update the presenter UI accordingly.

const presenterObserver = new MutationObserver(() => {
  const isDisconnected =
    document.documentElement.classList.contains("phx-disconnected");
  const overlay = document.getElementById("offline-overlay");
  const gridWrap = document.getElementById("grid-wrap");
  const statusDot = document.getElementById("status-dot");
  const statusText = document.getElementById("status-text");

  if (overlay) overlay.style.display = isDisconnected ? "flex" : "none";
  if (gridWrap) gridWrap.style.opacity = isDisconnected ? "0.15" : "1";
  if (statusDot) {
    statusDot.style.background = isDisconnected ? "#cf3f3f" : "#22c55e";
    statusDot.style.boxShadow = isDisconnected
      ? "0 0 6px #cf3f3f"
      : "0 0 6px #22c55e";
  }
  if (statusText) {
    statusText.textContent = isDisconnected ? "server offline" : "connected";
    statusText.style.color = isDisconnected ? "#cf3f3f" : "#5f4122";
  }
});

presenterObserver.observe(document.documentElement, {
  attributes: true,
  attributeFilter: ["class"],
});

const playObserver = new MutationObserver(() => {
  setPlayOfflineBanner(
    document.documentElement.classList.contains("phx-disconnected"),
  );
});
playObserver.observe(document.documentElement, {
  attributes: true,
  attributeFilter: ["class"],
});

// ─── Phoenix LiveSocket setup ─────────────────────────────────────────────────

const csrfToken = document
  .querySelector("meta[name='csrf-token']")
  .getAttribute("content");

const liveSocket = new LiveSocket("/live", Socket, {
  longPollFallbackMs: 2500,
  params: { _csrf_token: csrfToken },
  hooks: Hooks,
});

topbar.config({
  barColors: { 0: "#ef7c00" },
  shadowColor: "rgba(0, 0, 0, .3)",
});
window.addEventListener("phx:page-loading-start", (_info) => topbar.show(300));
window.addEventListener("phx:page-loading-stop", (_info) => topbar.hide());

// ─── Local LiveView (Popcorn/WASM) setup ─────────────────────────────────────

import { setup } from "local_live_view";
setup(liveSocket, { Socket, bundlePath: "bundle.avm" });

liveSocket.connect();
window.liveSocket = liveSocket;

// ─── Development helpers ──────────────────────────────────────────────────────

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
