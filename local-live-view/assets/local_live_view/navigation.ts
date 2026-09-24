import type { LLVConfig, LLVSocket } from "./types";
import type { PopcornClient } from "./index";

// Only the main LLV of the page - the one mounted by a `live_local` route -
// gets handle_params, like only the LiveView mounted at the router does in
// Phoenix. Any LLV can patch the URL, and the patch goes to whoever owns
// the page:
//
//  - Hosted: the page has a connected host LiveView (liveSocket.main). Phoenix owns
//    the browser history and the popstate handler, so we route LLV patches through
//    Phoenix (pushHistoryPatch), and the host LiveView handles them on the server.
//    There's no main LLV on such a page.
//
//  - Standalone: the page is rendered with no host LiveView.
//    There is no Phoenix popstate or [data-phx-link] click handler.
//    LLV must own navigation itself: intercept patch-link clicks, push the
//    history entry, handle popstate, and run handle_params in the main LLV.
export function registerNavigationHandlers(
  socket: LLVSocket,
  pop: PopcornClient,
  config: LLVConfig,
) {
  const absHref = (href: string) => new URL(href, window.location.origin).href;

  const phoenixOwnsNav = () => socket.isConnected();

  // Runs handle_params in the main LLV, if there's one
  const llvHandleParams = (href: string) => {
    pop.call({ action: "navigated", url: absHref(href) });
  };

  // Standalone-only: intercept clicks on patch links. Lets one `<.link patch>` work in
  // both modes, no separate LLV link component needed.
  document.addEventListener("click", (e: MouseEvent) => {
    if (phoenixOwnsNav()) return;

    const link = (e.target as Element).closest('a[data-phx-link="patch"]');
    if (!link) return;
    e.preventDefault();

    const to = link.getAttribute("href") ?? window.location.href;
    const replace = link.getAttribute("data-phx-link-state") === "replace";
    if (replace) {
      window.history.replaceState({ llv: true }, "", to);
    } else {
      window.history.pushState({ llv: true }, "", to);
    }
    llvHandleParams(to);
  });

  window.addEventListener("popstate", () => {
    if (phoenixOwnsNav()) return;
    llvHandleParams(window.location.href);
  });

  // llv:navigate: LLV push_patch fires this event. `handled` tells whether
  // handle_params already ran, which it did if the main LLV patched. We
  // write the history entry per mode:
  //  - hosted: hand to Phoenix via pushHistoryPatch (Phoenix-owned patch entry + host
  //    handle_params, on the server).
  //  - standalone: write the URL bar, then run handle_params in the main LLV,
  //    unless it's the one that patched.
  window.addEventListener("llv:navigate", (e: Event) => {
    const { href, replace, handled } = (
      e as CustomEvent<{ href: string; replace: boolean; handled: boolean }>
    ).detail;
    pop.call({ action: "url_changed", url: absHref(href) });

    if (config.onNavigate) {
      config.onNavigate(href, replace);
      return;
    }

    if (phoenixOwnsNav()) {
      socket.pushHistoryPatch(
        { isTrusted: false, type: "llv:navigate" },
        href,
        replace ? "replace" : "push",
        null,
      );
      return;
    }

    if (replace) {
      window.history.replaceState({ llv: true }, "", href);
    } else {
      window.history.pushState({ llv: true }, "", href);
    }
    if (!handled) llvHandleParams(href);
  });
}
