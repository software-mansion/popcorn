export function llvIdOf(el: HTMLElement): string {
  return el.dataset.popId ?? el.id;
}

export function resolveLlvId(viewOrId: string): string {
  const el = Array.from(document.querySelectorAll<HTMLElement>("[data-pop-view]")).find(
    (e) => e.getAttribute("data-pop-view") === viewOrId,
  );
  return el ? llvIdOf(el) : viewOrId;
}
