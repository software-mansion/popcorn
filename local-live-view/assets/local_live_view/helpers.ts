import type { LLVServerMessageDetail } from "./types";
import type { PopcornClient } from "./index";

// Resolve an LLV's element id from a view name or id: prefer the element whose
// data-pop-view matches, else fall back to treating the argument as an id.
export function resolveLlvId(viewOrId: string): string {
  const el = Array.from(document.querySelectorAll<HTMLElement>("[data-pop-view]")).find(
    (e) => e.getAttribute("data-pop-view") === viewOrId,
  );
  return el ? el.id : viewOrId;
}

// handles messages sent by the server via LocalLiveView.push_server_message
export function sendServerMessage(pop: PopcornClient, detail: LLVServerMessageDetail) {
  pop.serverMessage(resolveLlvId(detail.view), {
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
