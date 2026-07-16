import type {
  LLVSocket,
  LLVView,
  RenderedDiff,
  RefGenerator,
  EventPayload,
  ViewRegistry,
} from "./types";
import { LLV_DEFAULT_TARGET, LLV_SERVER_TARGET, LLV_TARGET_SEP } from "./types";
import { withHostLV } from "./helpers";
import type { PopcornClient } from "./index";

// Wire a fake Phoenix root view around a [data-pop-view] element so the
// browser renders the runtime's diffs and routes events to it.
export function setupFakeView(
  socket: LLVSocket,
  views: ViewRegistry,
  pop: PopcornClient,
  pop_view_el: HTMLElement,
  initialRendered: RenderedDiff,
) {
  const llvId = pop_view_el.id;

  const view = socket.newRootView(pop_view_el);
  views.set(llvId, view);

  // Handle LLV's custom targets (default, server, targets composed by LocalLiveView.targets/1)
  const origWithinTargets = view.withinTargets.bind(view);
  view.withinTargets = function (
    phxTarget: unknown,
    callback: (view: LLVView, ctx: null) => void,
    dom?: unknown,
  ) {
    const dispatchToken = (t: string) => {
      if (t === LLV_DEFAULT_TARGET) {
        callback(this, null);
      } else if (t === LLV_SERVER_TARGET) {
        withHostLV(socket, llvId, (hostView) => callback(hostView, null));
      } else {
        origWithinTargets(t, callback, dom);
      }
    };

    if (
      typeof phxTarget === "string" &&
      (phxTarget.includes(LLV_TARGET_SEP) ||
        phxTarget === LLV_DEFAULT_TARGET ||
        phxTarget === LLV_SERVER_TARGET)
    ) {
      for (const t of phxTarget.split(LLV_TARGET_SEP)) if (t) dispatchToken(t);
      return;
    }
    return origWithinTargets(phxTarget, callback, dom);
  };

  // addHook: skip the root element to prevent Phoenix from trying to register it
  // as a hook within this view's scope — hooks on children are still processed normally.
  const origAddHook = view.addHook.bind(view);
  view.addHook = function (this: LLVView, el: Element) {
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
  view.join = function (this: LLVView, callback?) {
    this.showLoader(this.liveSocket.loaderTimeout);
    this.joinCallback = (onDone) => {
      onDone = onDone ?? function () {};
      if (callback) {
        callback(this.joinCount, onDone);
      } else {
        onDone();
      }
    };
  };

  // pushWithReply: send event to Popcorn, return a resolved Promise so callers
  // like pushInput can safely chain .then() on it.
  // The actual diff arrives asynchronously via window.__popcornTransportReceive.
  view.pushWithReply = function (
    this: LLVView,
    refGenerator: RefGenerator | null,
    event: string,
    payload: EventPayload,
  ) {
    const [ref] = refGenerator ? refGenerator({ payload }) : [null, [], {}];

    if (typeof payload.cid !== "number") {
      delete payload.cid;
    }

    pop.event(this.el.id, payload);

    // In the real Phoenix flow, undoRefs is called synchronously inside
    // pushWithReply's channel reply handler — just before this.update(diff).
    // Since our diff arrives asynchronously via __popcornTransportReceive,
    // we must release the ref locks now. Otherwise DOMPatch sees PHX_REF_LOCK
    // on the form (set by pushInput for both the input AND the form element)
    // and applies all diff changes to a clone instead of the real DOM.
    if (ref !== null) {
      this.undoRefs(ref, (payload.event as string | undefined) ?? event);
    }

    return Promise.resolve({ resp: {}, reply: null, ref });
  };

  // maybePushComponentsDestroyed: component lifecycle is managed by Popcorn/WASM,
  // no need to notify the server about destroyed CIDs from the JS side
  view.maybePushComponentsDestroyed = function () {};

  view.bindChannel = function () {};
  view.join();

  if (initialRendered) {
    socket.requestDOMUpdate(() => view.onJoin({ rendered: initialRendered }));
  } else {
    console.error("LLV no initial rendered for view", llvId);
  }
}
