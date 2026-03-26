import { Popcorn } from "@swmansion/popcorn";

export async function setup(liveSocket, opts = {}) {
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
    debug: true,
    bundlePath: opts.bundlePath ?? "wasm/bundle.avm",
  });

  const { data: initialRenderedByView } = await popcorn.call(
    { views: find_predefined_views() },
    { timeoutMs: 10_000 },
  );

  const POP_VIEW = "data-pop-view";
  const pop_view_els = Array.from(document.querySelectorAll(`[${POP_VIEW}]`));

  // Map from view name → view instance, used by __popcornTransportReceive
  const viewsByName = {};

  window.__popcornTransportReceive = function (viewName, diff) {
    const view = viewsByName[viewName];
    if (!view) {
      console.error(
        "!!! LLV view not found:",
        viewName,
        "available:",
        Object.keys(viewsByName),
      );
      return;
    }
    view.update(diff, []);
  };

  pop_view_els.forEach((pop_view_el) => {
    const viewName = pop_view_el.getAttribute(POP_VIEW);

    const view = liveSocket.newRootView(pop_view_el);
    viewsByName[viewName] = view;

    // addHook: skip the root element — its phx-hook (e.g. ServerSendHook) was already
    // mounted by the parent Phoenix LiveView and has a HOOK_ID set on the element.
    // If we try to re-add it here Phoenix hits the "custom element" error path because
    // the hook exists in DOM private data but not in this view's viewHooks map.
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
    // data-phx-root-id and data-phx-session must be set so Phoenix LiveView treats the
    // element as mounted (the session value is unused locally but must be non-empty).
    view.join = function (callback) {
      this.el.setAttribute("data-phx-root-id", this.root.id);
      this.el.setAttribute("data-phx-session", this.root.id);
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

      const vName = this.el.dataset.popView;

      popcorn
        .call(
          { view: vName, event: event, payload: payload },
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

    const initialRendered = initialRenderedByView[viewName];
    if (initialRendered) {
      liveSocket.requestDOMUpdate(() =>
        view.onJoin({ rendered: initialRendered }),
      );
    } else {
      console.error("!!! LLV no initial rendered for view !!!", viewName);
    }
  });

  // Flush any server messages that arrived during Popcorn initialization.
  for (const detail of bufferedServerMessages) {
    await sendServerMessage(popcorn, detail);
  }
}

async function sendServerMessage(popcorn, detail) {
  await popcorn.call(
    {
      view: detail.view,
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
  return Array.from(document.querySelectorAll("[data-pop-view]")).map((el) =>
    el.getAttribute("data-pop-view"),
  );
}
