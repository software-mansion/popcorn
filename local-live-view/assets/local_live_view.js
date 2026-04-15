import { Popcorn } from "@swmansion/popcorn";

export async function setup(liveSocket, opts = {}) {
  const { Socket } = opts;
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

  popcorn = await Popcorn.init({
    debug: opts.debug ?? false,
    bundlePaths: opts.bundlePaths ?? ["wasm/bundle.avm"],
  });
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

  const { data: initialRenderedByView } = await popcorn.call(
    { views: find_predefined_views() },
    { timeoutMs: 30_000 },
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
    view.update(diff, []);
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
        onDone = onDone || function () {};
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
    view.maybePushComponentsDestroyed = function (_destroyedCIDs) {};

    view.bindChannel = function () {};
    view.join();

    const initialRendered = initialRenderedByView[llvId];
    if (initialRendered) {
      liveSocket.requestDOMUpdate(() =>
        view.onJoin({ rendered: initialRendered }),
      );
    } else {
      console.error("LLV no initial rendered for view", llvId);
    }

    // OrderLive's DOM patch on reconnect strips data-phx-root-id from the LLV
    // container element, causing Phoenix to route LLV click events to OrderLive
    // instead of Popcorn. Watch for the attribute being cleared and restore it
    // immediately so the LLV keeps ownership of its subtree.
    const observer = new MutationObserver(() => {
      if (!pop_view_el.getAttribute("data-phx-session")) {
        pop_view_el.setAttribute("data-phx-session", pop_view_el.id);
      }
    });
    observer.observe(pop_view_el, {
      attributes: true,
      attributeFilter: ["data-phx-root-id", "data-phx-session"],
    });
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

  // Flush any server messages that arrived during Popcorn initialization.
  for (const detail of bufferedServerMessages) {
    await sendServerMessage(popcorn, detail);
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

function find_predefined_views() {
  return Array.from(document.querySelectorAll("[data-pop-view]")).map((el) => ({
    view: el.getAttribute("data-pop-view"),
    id: el.id,
  }));
}
