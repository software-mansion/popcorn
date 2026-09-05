// The canonical view id of a mount point. The Component writes it in
// data-pop-id: the id itself belongs to the root div inside the mount
// point (it is the LiveView's id, its channel topic, and the key of every
// LLV registry). The el.id fallback covers markup predating data-pop-id.
export function llvIdOf(el: HTMLElement): string {
  return el.dataset.popId ?? el.id;
}

// Resolve an LLV's canonical id from a view name or id: prefer the mount
// point whose data-pop-view matches, else fall back to treating the
// argument as an id.
export function resolveLlvId(viewOrId: string): string {
  const el = Array.from(document.querySelectorAll<HTMLElement>("[data-pop-view]")).find(
    (e) => e.getAttribute("data-pop-view") === viewOrId,
  );
  return el ? llvIdOf(el) : viewOrId;
}
