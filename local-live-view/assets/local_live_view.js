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

    // Register the server message listener immediately — BEFORE any awaits.
    // push_event("llv_server_message") from Phoenix LiveView fires during the initial
    // LiveView join, which happens before Popcorn finishes initializing. Without this,
    // the event is dispatched on window before our listener is registered and is lost.
    let popcorn;
    const bufferedServerMessages = [];

    window.addEventListener("phx:llv_server_message", async (e) => {
      if (!popcorn) {
        bufferedServerMessages.push(e.detail);
        return;
      }
      await sendServerMessage(popcorn, e.detail);
    });

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

    if (config.eventHandler) {
      popcorn.onMessage(config.eventHandler);
    }

    window.__popcorn = popcorn;

    const channels = {};
    const viewsById = {};

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
                  { id: llvId, event: "llv_reconnected", payload: {} },
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

    const initialUrlParams = Object.fromEntries(
      new URLSearchParams(window.location.search).entries(),
    );
    const initialUrl = window.location.href;

    const { data: initialRenderedByView } = await popcorn.call(
      { views: findPredefinedViews(), params: initialUrlParams, url: initialUrl },
      { timeoutMs: 10_000 },
    );

    const pop_view_els = Array.from(document.querySelectorAll("[data-pop-view]"));

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

    pop_view_els.forEach((pop_view_el) => {
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
            { id: this.el.id, event: event, payload: payload },
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

      const initialRendered = initialRenderedByView[llvId];
      if (initialRendered) {
        liveSocket.requestDOMUpdate(() =>
          view.onJoin({ rendered: initialRendered }),
        );
      } else {
        console.error("LLV no initial rendered for view", llvId);
      }
    });

    const absHref = (href) => new URL(href, window.location.origin).href;

    // LLV navigation runs in one of two modes:
    //
    //  - Hosted: the page has a connected host LiveView (liveSocket.main). Phoenix owns
    //    the browser history and the popstate handler, so we route LLV navigation through
    //    Phoenix (pushHistoryPatch) and just listen to phx:navigate to re-run handle_params
    //    in the LLV views. This keeps the host LV and history in sync
    //
    //  - Standalone: the page is rendered with no host LiveView.
    //    There is no Phoenix popstate or [data-phx-link] click handler.
    //    LLV must own navigation itself: intercept patch-link clicks, push the
    //    history entry, and handle popstate.
    const phoenixOwnsNav = () => liveSocket.isConnected();

    const llvHandleParams = (href) => {
      const url = absHref(href);
      const params = Object.fromEntries(new URL(url).searchParams.entries());

      for (const llvId of Object.keys(viewsById)) {
        popcorn
          .call(
            { id: llvId, event: "handle_params", payload: { params, url } },
            { timeoutMs: 10_000 },
          )
          .catch((err) => console.error("LLV handle_params error", err));
      }
    };

    let lastLLVNavigatedHref = null;

    // Standalone-only: intercept clicks on patch links. Lets one `<.link patch>` work in
    // both modes, no separate LLV link component needed.
    document.addEventListener("click", (e) => {
      if (phoenixOwnsNav()) return;

      const link = e.target.closest('a[data-phx-link="patch"]');
      if (!link) return;
      e.preventDefault();

      const to = link.getAttribute("href");
      const replace = link.getAttribute("data-phx-link-state") === "replace";
      if (replace) {
        window.history.replaceState({ llv: true }, "", to);
      } else {
        window.history.pushState({ llv: true }, "", to);
      }
      llvHandleParams(to);
    });

    window.addEventListener("popstate", () => {
      if (phoenixOwnsNav()) return;
      llvHandleParams(window.location.href);
    });

    // llv:navigate: LLV push_patch fires this event after the WASM-side
    // handle_params has run. We write the history entry per mode:
    //  - hosted: hand to Phoenix via pushHistoryPatch (Phoenix-owned patch entry + host
    //    handle_params); the phx:navigate echo is skipped via lastLLVNavigatedHref.
    //  - standalone: just write the URL bar — the WASM side already ran handle_params.
    window.addEventListener("llv:navigate", (e) => {
      const href = e.detail.href;
      lastLLVNavigatedHref = absHref(href);

      if (config.onNavigate) {
        config.onNavigate(href, e.detail.replace);
        return;
      }

      if (phoenixOwnsNav()) {
        liveSocket.pushHistoryPatch(
          { isTrusted: false, type: "llv:navigate" },
          href,
          e.detail.replace ? "replace" : "push",
          null,
        );
      } else if (e.detail.replace) {
        window.history.replaceState({ llv: true }, "", href);
      } else {
        window.history.pushState({ llv: true }, "", href);
      }
    });

    // phx:navigate: forward Phoenix LiveView patch navigations to all LLV views (hosted
    // mode only — never dispatched in dead mode). Fires for <.link patch> clicks and
    // browser back/forward. Skip navigations LLV itself triggered via push_patch, since
    // LLV already ran handle_params on the WASM side for those.
    window.addEventListener("phx:navigate", (e) => {
      if (!e.detail?.patch) return;

      const url = absHref(e.detail.href ?? window.location.href);
      if (url === lastLLVNavigatedHref) {
        lastLLVNavigatedHref = null;
        return;
      }

      llvHandleParams(url);
    });

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
      { id: llvId, event: "llv_push", payload: { event, payload } },
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
      event: "llv_server_message",
      payload: {
        event: "llv_server_message",
        value: detail.payload,
        type: "llv_server_message",
      },
    },
    { timeoutMs: 10_000 },
  );
}

function findPredefinedViews() {
  return Array.from(document.querySelectorAll("[data-pop-view]")).map((el) => ({
    view: el.getAttribute("data-pop-view"),
    id: el.id,
  }));
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
