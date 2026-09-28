import type { EventBusHook, LLVSocket } from "./types";
import type { PopcornClient } from "./index";
import { llvIdOf } from "./helpers";

export function registerNavigationHandlers(
  pop: PopcornClient,
  socket: LLVSocket,
  getAnyHook: () => EventBusHook | undefined,
) {
  // Intercept clicks on patch links. Lets one `<.link patch>` work on any
  // page, no separate LLV link component needed.
  document.addEventListener("click", (e: MouseEvent) => {
    const mainId = mainViewId();
    if (!mainId) return;

    const link = (e.target as Element).closest<HTMLElement>('a[data-phx-link="patch"]');
    if (!link) return;
    // For patch links, prevent the LV handlers from handling the event
    // as they would reload the page if there's no main LV on the server
    e.preventDefault();
    e.stopImmediatePropagation();

    const to = link.getAttribute("href") ?? window.location.href;
    writeHistory(to, link.getAttribute("data-phx-link-state") === "replace");
    handleParams(pop, mainId, to);

    // Preventing the event propagation breaks phx-click handling,
    // so it's triggered here manually.
    const phxClick = link.getAttribute("phx-click");
    if (phxClick) socket.execJS(link, phxClick, "click");
  });

  // Runs before Phoenix's popstate handler, because
  // it's registered first: LLVEngine.create must be called before
  // liveSocket.connect(), which registers Phoenix's.
  window.addEventListener("popstate", (e: PopStateEvent) => {
    const mainId = mainViewId();
    if (!mainId) return;
    // Prevent the LV handlers from handling the event,
    // as they would reload the page if there's no main LV on the server
    e.stopImmediatePropagation();
    handleParams(pop, mainId, window.location.href);
  });

  // llv:navigate: an LLV called push_patch
  window.addEventListener("llv:navigate", (e: Event) => {
    const { href, replace } = (e as CustomEvent<{ href: string; replace: boolean }>).detail;

    const mainId = mainViewId();
    if (mainId) {
      writeHistory(href, replace);
      handleParams(pop, mainId, href);
      return;
    }

    // Hooks only mount inside a LiveView, so there's none on a page without one
    const hook = getAnyHook();
    if (hook) {
      hook.js().patch(href, { replace });
    } else if (replace) {
      window.location.replace(href);
    } else {
      window.location.assign(href);
    }
  });
}

// The main LLV owns the page's navigation. Its mount point is in the
// server-rendered page from the start.
function mainViewId(): string | null {
  const el = document.querySelector<HTMLElement>("[data-pop-main]");
  return el ? llvIdOf(el) : null;
}

function writeHistory(href: string, replace: boolean): void {
  if (replace) {
    window.history.replaceState(null, "", href);
  } else {
    window.history.pushState(null, "", href);
  }
}

// Runs handle_params in the main LLV. The dispatcher queues it if the view
// is still mounting.
function handleParams(pop: PopcornClient, mainId: string, href: string): void {
  pop.call({
    action: "dispatch_to_view",
    id: mainId,
    payload: { action: "handle_params", url: new URL(href, window.location.href).href },
  });
}
