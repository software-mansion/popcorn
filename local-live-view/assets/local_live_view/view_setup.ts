import type { Socket as PhoenixSocket } from "phoenix";
import type { LLVSocket, LLVView, ViewRegistry } from "./types";

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
  // No need to set data-phx-session / data-phx-root-id: owner() is
  // overridden by the engine to route events via [data-pop-view] lookup
  // instead of attribute-based lookup.
  view.join();
}
