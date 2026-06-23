import { Popcorn } from "@swmansion/popcorn";

/**
 * @typedef {Object} LLVConfig
 * @property {typeof import("phoenix").Socket} [Socket] - The Phoenix Socket class (required when using mirror channels).
 * @property {string[]} [bundlePaths] - Paths to the compiled WASM bundle files. Defaults to `["wasm/bundle.avm"]`.
 * @property {boolean} [debug] - Enable Popcorn debug logging. Defaults to `false`.
 * @property {(msg: unknown) => void} [eventHandler] - Optional callback for raw Popcorn messages.
 * @property {(href: string, replace: boolean) => void} [onNavigate] - Override LLV's default navigation handler.
 *   Called instead of `liveSocket.historyPatch` when an LLV view calls `push_patch`.
 *   The default syncs LLV navigation into the Phoenix LiveView channel so the host page's
 *   `handle_params/3` fires. Pass a custom function to take full control of navigation.
 */

export class LLVEngine {
  /** @param {unknown} popcorn */
  constructor(popcorn) {
    this.popcorn = popcorn;
  }

  /**
   * Initializes LLVEngine and connects the LiveSocket.
   *
   * @param {import("phoenix_live_view").LiveSocket} liveSocket
   * @param {LLVConfig} [config]
   * @returns {Promise<LLVEngine>}
   */
  static async create(liveSocket, config = {}) {
    const { Socket } = config;

    let popcorn;
    // Whether the Popcorn runtime has finished booting. Hooks that fire before it
    // does can't mount (Popcorn isn't there yet); the startup scan picks those up.
    let popcornReady = false;
    const channels = {};
    const viewsById = {};
    const bufferedServerMessages = [];

    // Wire a fake Phoenix root view around a [data-pop-view] element so the
    // browser renders the runtime's diffs and routes events to it.
    const setupFakeView = (pop_view_el, initialRendered) => {
      const llvId = pop_view_el.id;

      const view = liveSocket.newRootView(pop_view_el);
      viewsById[llvId] = view;

      // addHook: skip the root element to prevent Phoenix from trying to register it
      // as a hook within this view's scope — hooks on children are still processed normally.
      const origAddHook = view.addHook.bind(view);
      view.addHook = function (el) {
        if (el === this.el) return;
        return origAddHook(el);
      };

      // isConnected: we are always "connected" to Popcorn
      view.isConnected = function () {
        return true;
      };

      // join: skip bindChannel + channel.join — onJoin is called below after Popcorn mounts.
      // No need to set data-phx-session / data-phx-root-id: owner() is overridden below
      // to route events via [data-pop-view] lookup instead of attribute-based lookup.
      view.join = function (callback) {
        this.showLoader(this.liveSocket.loaderTimeout);
        this.joinCallback = (onDone) => {
          onDone = onDone || function () { };
          callback ? callback(this.joinCount, onDone) : onDone();
        };
      };

      // pushWithReply: send event to Popcorn, return a resolved Promise so callers
      // like pushInput can safely chain .then() on it.
      // The actual diff arrives asynchronously via window.__popcornTransportReceive.
      view.pushWithReply = function (refGenerator, event, payload) {
        const [ref, [_el], _opts] = refGenerator
          ? refGenerator({ payload })
          : [null, [], {}];

        if (typeof payload.cid !== "number") {
          delete payload.cid;
        }

        popcorn
          .call(
            { action: "event", id: this.el.id, payload: payload },
            { timeoutMs: 10_000 },
          )
          .catch((err) => console.error("LLV view.pushWithReply error", err));

        // In the real Phoenix flow, undoRefs is called synchronously inside
        // pushWithReply's channel reply handler — just before this.update(diff).
        // Since our diff arrives asynchronously via __popcornTransportReceive,
        // we must release the ref locks now. Otherwise DOMPatch sees PHX_REF_LOCK
        // on the form (set by pushInput for both the input AND the form element)
        // and applies all diff changes to a clone instead of the real DOM.
        if (ref !== null) {
          this.undoRefs(ref, payload.event || event);
        }

        return Promise.resolve({ resp: {}, reply: null, ref });
      };

      // maybePushComponentsDestroyed: component lifecycle is managed by Popcorn/WASM,
      // no need to notify the server about destroyed CIDs from the JS side
      view.maybePushComponentsDestroyed = function (_destroyedCIDs) { };

      view.bindChannel = function () { };
      view.join();

      if (initialRendered) {
        liveSocket.requestDOMUpdate(() =>
          view.onJoin({ rendered: initialRendered }),
        );
      } else {
        console.error("LLV no initial rendered for view", llvId);
      }
    };

    // Stop a view's runtime process. The caller drops the fake view (if any).
    const teardownProcess = (llvId) =>
      popcorn
        .call({ action: "destroy", id: llvId, payload: {} }, { timeoutMs: 10_000 })
        .catch((err) => console.error("LLV destroy error", err));

    // Start a view and wire it up.
    const mountView = async (pop_view_el) => {
      const llvId = pop_view_el.id;
      if (viewsById[llvId]) return;
      const { data } = await popcorn.call(
        { action: "create", id: llvId, view: pop_view_el.getAttribute("data-pop-view") },
        { timeoutMs: 10_000 },
      );
      if (data.status == "error") return;
      const liveEl = document.getElementById(llvId);
      if (liveEl?.matches("[data-pop-view]")) {
        setupFakeView(liveEl, data.rendered);
      } else {
        teardownProcess(llvId);
      }
    };

    // Stop a view's runtime process and drop its fake view. Used by the hook when
    // the host LiveView removes the mount point.
    const unmountView = (pop_view_el) => {
      const llvId = pop_view_el.id;
      const view = viewsById[llvId];
      if (!view) return;
      delete viewsById[llvId];
      view.destroy?.();
      teardownProcess(llvId);
    };

    // Register the server message listener immediately — BEFORE any awaits.
    // push_event("llv_server_message") from Phoenix LiveView fires during the initial
    // LiveView join, which happens before Popcorn finishes initializing. Without this,
    // the event is dispatched on window before our listener is registered and is lost.
    window.addEventListener("phx:llv_server_message", async (e) => {
      if (!popcorn) {
        bufferedServerMessages.push(e.detail);
        return;
      }
      await sendServerMessage(popcorn, e.detail);
    });

    // Hook that manages views rendered inside a host LiveView.
    // Other views are rendered during startup scan.
    // The startup scan also handles the case when hook's mount
    // fires before Popcorn is ready.
    liveSocket.hooks.LocalLiveView = {
      mounted() {
        if (popcornReady) mountView(this.el);
      },
      destroyed() {
        unmountView(this.el);
      },
    };

    // Pages with only LocalLiveViews (no server-side LiveView) connect in "dead"
    // mode, which skips bindForms() — making phx-submit / phx-change no-ops on
    // any LLV. Wire them up manually when no real LiveView is on the page.
    if (!document.querySelector("[data-phx-session]")) {
      liveSocket.bindForms();
    }

    popcorn = await Popcorn.init({
      debug: config.debug ?? false,
      bundlePaths: config.bundlePaths ?? ["wasm/bundle.avm"],
    });
    popcornReady = true;

    if (config.eventHandler) {
      popcorn.onMessage(config.eventHandler);
    }

    window.__popcorn = popcorn;

    // Mirror channels: only created for views with a server-side Mirror module.
    const mirrorEls = document.querySelectorAll(
      "[data-pop-view][data-pop-mirror]",
    );
    if (mirrorEls.length > 0) {
      const csrfToken = document
        .querySelector("meta[name='csrf-token']")
        ?.getAttribute("content");

      const llvSocket = new Socket("/llv_socket", {
        params: { _csrf_token: csrfToken },
      });
      llvSocket.connect();

      mirrorEls.forEach((el) => {
        const llvId = el.id;
        const channel = llvSocket.channel(`llv:${llvId}`, {
          view: el.dataset.popView,
        });
        channels[llvId] = channel;

        channel
          .join()
          .receive("ok", () => {
            if (viewsById[llvId]) {
              popcorn
                .call(
                  { action: "reconnected", id: llvId, payload: {} },
                  { timeoutMs: 10_000 },
                )
                .catch((err) => console.error("LLV reconnect sync error", err));
            }
          })
          .receive("error", (err) =>
            console.error("LLV channel join error", err),
          );
      });

      window.__llvSync = (id, eventName, payload) => {
        const channel = channels[id];
        if (channel) {
          channel.push(eventName, payload);
        }
      };
    }

    window.__popcornTransportReceive = function (llvId, diff) {
      const view = viewsById[llvId];
      if (!view) {
        console.error(
          "LLV view not found:",
          llvId,
          "available:",
          Object.keys(viewsById),
        );
        return;
      }
      // The diff is delivered from the Popcorn WASM iframe via run_js, so its
      // arrays belong to the iframe's JS realm. phoenix_live_view's isObject()
      // uses `!(obj instanceof Array)`, which is false for cross-realm arrays —
      // so it treats statics arrays as plain objects. In the component-only diff
      // path (cloneMerge), that turns a cached statics array into `{0:.., 1:..}`
      // with no `.length`, and the renderer then emits only `statics[0]`, so the
      // re-rendered component is truncated. structuredClone re-creates the diff
      // in this realm so `instanceof Array` works again.
      view.update(structuredClone(diff), []);
    };

    // owner: route events from inside [data-pop-view] elements to our fake views.
    // We never set data-phx-session on LLV elements, so Phoenix's default closestViewEl()
    // would walk up to the parent LiveView and dispatch events there instead.
    const origOwner = liveSocket.owner.bind(liveSocket);
    liveSocket.owner = function (childEl, callback) {
      const llvEl = childEl.closest?.("[data-pop-view]");
      if (llvEl && viewsById[llvEl.id]) {
        const view = viewsById[llvEl.id];
        return callback ? callback(view) : view;
      }
      return origOwner(childEl, callback);
    };

    registerCustomEventBindings(liveSocket);

    // Startup scan: mount every [data-pop-view] present now that Popcorn is up.
    // This is the mount path for host-less pages (no hooks fire there) and the
    // catch-up for hooks that fired before Popcorn was ready.
    // If a view is mounted twice (here and by the hook), the dispatcher
    // on the Elixir side ignores the second mount.
    const pop_view_els = Array.from(
      document.querySelectorAll("[data-pop-view]"),
    );
    await Promise.all(pop_view_els.map((el) => mountView(el)));

    // Flush any server messages that arrived during Popcorn initialization.
    for (const detail of bufferedServerMessages) {
      await sendServerMessage(popcorn, detail);
    }

    return new LLVEngine(popcorn);
  }

  /**
   * Pushes an event into a LLVEngine from external JavaScript.
   *
   * @param {string} viewId - The view name (e.g. `"ThermostatLive"`) or element id.
   * @param {string} event - The event name to dispatch into the view's `handle_info/2`.
   * @param {object} [payload] - Optional payload map passed alongside the event.
   * @returns {Promise<void>}
   */
  async pushEvent(viewId, event, payload = {}) {
    const el = document.querySelector(`[data-pop-view="${viewId}"]`);
    const llvId = el ? el.id : viewId;

    const result = await this.popcorn.call(
      { action: "push", id: llvId, payload: { event, payload } },
      { timeoutMs: 10_000 },
    );

    if (!result.ok) {
      console.error(
        `LLV pushEvent error for view "${viewId}", event "${event}":`,
        result,
      );
    }
  }
}

async function sendServerMessage(popcorn, detail) {
  const el = document.querySelector(`[data-pop-view="${detail.view}"]`);
  const llvId = el ? el.id : detail.view;

  await popcorn.call(
    {
      id: llvId,
      action: "event",
      payload: {
        event: "llv_server_message",
        value: detail.payload,
        type: "llv_server_message",
      },
    },
    { timeoutMs: 10_000 },
  );
}

// Phoenix LiveView only binds click/keydown/keyup/blur/focus natively, so
// phx-mouse*/phx-window-mouse* and the HTML5 phx-drag* bindings are no-ops.
// Wire them up here, walking up from e.target to find the closest ancestor with
// the binding (same as how phx-click works) so element-level bindings on
// container elements also catch events from their descendants.
function registerCustomEventBindings(liveSocket) {
  // The binding element's bounding rect is included so handlers can
  // position-aware-route the event (e.g. clientY - rect.top for the offset
  // within the element, rect.height for its size).
  const buildPointerData = (e, el) => {
    const rect = el.getBoundingClientRect();
    return {
      clientX: e.clientX,
      clientY: e.clientY,
      pageX: e.pageX,
      pageY: e.pageY,
      screenX: e.screenX,
      screenY: e.screenY,
      movementX: e.movementX,
      movementY: e.movementY,
      button: e.button,
      buttons: e.buttons,
      altKey: e.altKey,
      ctrlKey: e.ctrlKey,
      metaKey: e.metaKey,
      shiftKey: e.shiftKey,
      rect: {
        top: rect.top,
        left: rect.left,
        width: rect.width,
        height: rect.height,
      },
    };
  };

  // Walk up from `target` to the nearest element that carries the `binding`
  // attribute (e.g. "phx-dragover"), returning it or null if none is found.
  // Mirrors how phx-click resolves a handler from the event's deepest target.
  const closestWithBinding = (target, binding) => {
    let el = target;
    while (
      el &&
      el.nodeType === 1 &&
      !(el.getAttribute && el.getAttribute(binding))
    ) {
      el = el.parentNode;
    }
    return el && el.nodeType === 1 && el.getAttribute(binding) ? el : null;
  };

  const mouseEventTypes = [
    "mousedown",
    "mouseup",
    "mousemove",
    "mouseover",
    "mouseout",
  ];
  for (const eventType of mouseEventTypes) {
    const elementBinding = `phx-${eventType}`;
    const windowBinding = `phx-window-${eventType}`;
    window.addEventListener(eventType, (e) => {
      const el = closestWithBinding(e.target, elementBinding);
      if (el) {
        const phxEvent = el.getAttribute(elementBinding);
        liveSocket.debounce(el, e, eventType, () => {
          liveSocket.withinOwners(el, (view) => {
            view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
          });
        });
      } else {
        document.querySelectorAll(`[${windowBinding}]`).forEach((wel) => {
          const phxEvent = wel.getAttribute(windowBinding);
          liveSocket.debounce(wel, e, eventType, () => {
            liveSocket.withinOwners(wel, (view) => {
              view.pushEvent(eventType, wel, null, phxEvent, buildPointerData(e, wel), {});
            });
          });
        });
      }
    });
  }

  // HTML5 drag-and-drop. The browser only allows a drop — and shows the move
  // cursor — when dragenter/dragover call preventDefault() on a drop target,
  // and drop must preventDefault() too. dragstart seeds dataTransfer so Firefox
  // actually initiates the drag. Bindings are read off phx-drag* attributes and
  // dispatched just like the mouse events above, with the same pointer data so
  // handlers can use the rect to decide drop position.
  const dragEventTypes = [
    "dragstart",
    "dragenter",
    "dragover",
    "dragleave",
    "drop",
    "dragend",
  ];
  const dragPreventDefault = new Set(["dragenter", "dragover", "drop"]);
  for (const eventType of dragEventTypes) {
    const elementBinding = `phx-${eventType}`;
    window.addEventListener(eventType, (e) => {
      const el = closestWithBinding(e.target, elementBinding);
      if (!el) return;

      // preventDefault must run on every event (not just the throttled ones)
      // or the browser keeps rejecting the drop mid-drag.
      if (dragPreventDefault.has(eventType)) {
        e.preventDefault();
      }
      if (e.dataTransfer) {
        if (eventType === "dragstart") {
          e.dataTransfer.effectAllowed = "move";
          try {
            e.dataTransfer.setData("text/plain", el.id || "");
          } catch (_) { }
        } else if (eventType === "dragover") {
          e.dataTransfer.dropEffect = "move";
        }
      }

      const phxEvent = el.getAttribute(elementBinding);
      liveSocket.debounce(el, e, eventType, () => {
        liveSocket.withinOwners(el, (view) => {
          view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
        });
      });
    });
  }
}
