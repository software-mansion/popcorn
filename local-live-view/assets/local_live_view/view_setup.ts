import type { Socket as PhoenixSocket } from "phoenix";
import type { LLVSocket, LLVView, ViewRegistry } from "./types";

// The attributes that make LiveView's stock element→view resolution find the
// fake view: closestViewEl stops at the nearest [data-phx-session] element,
// and getViewByEl resolves data-phx-root-id against liveSocket.roots — where
// newRootView registers the fake view under the element's id. The session
// value is never read (the transport answers the join without it). Scoping
// keyed on these attrs (ownsElement, form recovery, focus tracking) also
// excludes LLV DOM from the host view.
//
// A host patch of the mount element STRIPS these attrs: mergeAttrs on a
// phx-update="ignore" element removes every data-* attribute the server HTML
// doesn't carry. The same patch always fires the element's updated() hook in
// the same task, so the LocalLiveView hook re-asserts them there — no event
// can dispatch inside the gap.
export function setPhxResolutionAttrs(el: HTMLElement) {
  el.setAttribute("data-phx-session", "");
  el.setAttribute("data-phx-root-id", el.id);
}

// Wire a fake Phoenix root view around a [data-pop-view] element so the
// browser renders the runtime's diffs and routes events to it.
export function setupFakeView(
  socket: LLVSocket,
  views: ViewRegistry,
  popcornSocket: PhoenixSocket,
  pop_view_el: HTMLElement,
) {
  const llvId = pop_view_el.id;

  const view = socket.newRootView(pop_view_el);
  views.set(llvId, view);

  // The view's channel: a real Phoenix Channel on the popcorn socket.
  // Everything channel-shaped — join handshake, event acks carrying
  // diffs, out-of-band "diff" frames, ref bookkeeping — runs through the
  // stock Channel/Push machinery over the PopcornTransport.
  view.channel = popcornSocket.channel(`lv:${llvId}`);

  // Participate in LiveView's stock element→view resolution instead of
  // patching liveSocket.owner (see setPhxResolutionAttrs).
  //
  // MUST stay client-side and AFTER newRootView: joinRootViews (which runs
  // at liveSocket.connect(), before Popcorn boots) adopts any
  // [data-phx-session] element whose id is NOT already in liveSocket.roots
  // as a real LiveView on the real websocket — a server-rendered session
  // attr would hand the mount points to Phoenix. And never set
  // data-phx-parent-id, or the host view would adopt them as children.
  setPhxResolutionAttrs(pop_view_el);

  // The view's OWN join patch also strips the attrs (DOMPatch merges the
  // container's attrs from the rendered HTML, which doesn't carry them) —
  // and unlike a host patch, it fires no updated() hook to re-assert them.
  // Watch the element and re-assert whenever anything drops the session
  // attr; the observer fires in a microtask, before any event can dispatch.
  const observer = new MutationObserver(() => {
    if (!pop_view_el.hasAttribute("data-phx-session")) setPhxResolutionAttrs(pop_view_el);
  });
  observer.observe(pop_view_el, {
    attributes: true,
    attributeFilter: ["data-phx-session", "data-phx-root-id"],
  });

  const origDestroy = view.destroy?.bind(view);
  view.destroy = (callback?: () => void) => {
    observer.disconnect();
    origDestroy?.(callback);
  };

  // addHook: skip the root element to prevent Phoenix from trying to register it
  // as a hook within this view's scope — hooks on children are still processed normally.
  const origAddHook = view.addHook.bind(view);
  view.addHook = function (this: LLVView, el: Element) {
    if (el === this.el) return;
    return origAddHook(el);
  };

  // Stock join: bindChannel + channel.join over the popcorn transport.
  // The join frame is answered by the view's process itself, serving the
  // rendered it produced at mount — so onJoin runs the regular path.
  view.join();
}
