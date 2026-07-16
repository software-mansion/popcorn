import type { LLVSocket, LLVView, LLVServerMessageDetail } from "./types";
import { PHX_VIEW_SELECTOR } from "./types";
import type { PopcornClient } from "./index";

// Resolve an LLV's element id from a view name or id: prefer the element whose
// data-pop-view matches, else fall back to treating the argument as an id.
export function resolveLlvId(viewOrId: string): string {
  const el = Array.from(document.querySelectorAll<HTMLElement>("[data-pop-view]")).find(
    (e) => e.getAttribute("data-pop-view") === viewOrId,
  );
  return el ? el.id : viewOrId;
}

// Resolve an LLV's host LiveView fresh on every call (a reconnect can swap the
// [data-phx-session] element) and hand it to `fun`. We can't use
// liveSocket.withinOwners(llvElement, fun) here: overriding the owner
// connects [data-pop-view] elements with the fake local view, so we hop to the host
// element ourselves first.
export function withHostLV(
  socket: LLVSocket,
  llvId: string,
  fun: (hostView: LLVView, hostEl: Element) => void,
) {
  const llvElement = document.getElementById(llvId);
  if (!llvElement) {
    console.error("LLV withHostLV: LLV element not found", llvId);
    return;
  }
  const hostEl = llvElement.closest(PHX_VIEW_SELECTOR);
  if (!hostEl) {
    console.error("LLV withHostLV: no host LiveView for view", llvId);
    return;
  }
  // liveSocket.owner only calls back when the element maps to a live view;
  // surface the miss instead of dropping the event without a trace.
  let dispatched = false;
  socket.owner(hostEl, (hostView: LLVView) => {
    dispatched = true;
    fun(hostView, hostEl);
  });
  if (!dispatched) {
    console.error("LLV withHostLV: host LiveView element has no live view", { llvId, hostEl });
  }
}

export function sendServerMessage(pop: PopcornClient, detail: LLVServerMessageDetail) {
  pop.event(resolveLlvId(detail.view), {
    event: "llv_server_message",
    value: detail.payload,
    type: "llv_server_message",
  });
}

// data-pop-assigns holds the JSON a local_component serialized from its inline
// assigns. Absent/empty for plain local views — default to {} so the process is
// always handed a map.
export function parseAssigns(raw: string | null): Record<string, unknown> {
  if (!raw) return {};
  try {
    return JSON.parse(raw);
  } catch (err) {
    console.error("LLV failed to parse data-pop-assigns:", raw, err);
    return {};
  }
}
