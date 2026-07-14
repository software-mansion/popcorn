import type { LLVConfig, LLVSocket } from "./types";
import type { PopcornClient } from "./core/popcorn_client";
import type { ViewRegistry } from "./core/view_registry";

// LLV navigation runs in one of two modes:
//
//  - Hosted: the page has a connected host LiveView (liveSocket.main). Phoenix owns
//    the browser history and the popstate handler, so we route LLV navigation through
//    Phoenix (pushHistoryPatch) and just listen to phx:navigate to re-run handle_params
//    in the LLV views. This keeps the host LV and history in sync
//
//  - Standalone: the page is rendered with no host LiveView.
//    There is no Phoenix popstate or [data-phx-link] click handler.
//    LLV must own navigation itself: intercept patch-link clicks, push the
//    history entry, and handle popstate.
export function registerNavigationHandlers(
  socket: LLVSocket,
  views: ViewRegistry,
  pop: PopcornClient,
  config: LLVConfig,
) {
  const absHref = (href: string) => new URL(href, window.location.origin).href;

  const phoenixOwnsNav = () => socket.isConnected();

  const llvHandleParams = (href: string) => {
    const url = absHref(href);
    const params = Object.fromEntries(new URL(url).searchParams.entries());
    for (const llvId of views.ids()) {
      pop.handleParams(llvId, params, url);
    }
  };

  let lastLLVNavigatedHref: string | null = null;

  // Standalone-only: intercept clicks on patch links. Lets one `<.link patch>` work in
  // both modes, no separate LLV link component needed.
  document.addEventListener("click", (e: MouseEvent) => {
    if (phoenixOwnsNav()) return;

    const link = (e.target as Element).closest('a[data-phx-link="patch"]');
    if (!link) return;
    e.preventDefault();

    const to = link.getAttribute("href") ?? "/";
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

  // llv:navigate: LLV push_patch fires this event after the WASM-side
  // handle_params has run. We write the history entry per mode:
  //  - hosted: hand to Phoenix via pushHistoryPatch (Phoenix-owned patch entry + host
  //    handle_params); the phx:navigate echo is skipped via lastLLVNavigatedHref.
  //  - standalone: just write the URL bar — the WASM side already ran handle_params.
  window.addEventListener("llv:navigate", (e: Event) => {
    const { href, replace } = (e as CustomEvent<{ href: string; replace: boolean }>).detail;
    lastLLVNavigatedHref = absHref(href);

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
    } else if (replace) {
      window.history.replaceState({ llv: true }, "", href);
    } else {
      window.history.pushState({ llv: true }, "", href);
    }
  });

  // phx:navigate: forward Phoenix LiveView patch navigations to all LLV views (hosted
  // mode only — never dispatched in dead mode). Fires for <.link patch> clicks and
  // browser back/forward. Skip navigations LLV itself triggered via push_patch, since
  // LLV already ran handle_params on the WASM side for those.
  window.addEventListener("phx:navigate", (e: Event) => {
    const detail = (e as CustomEvent<{ href?: string; patch?: boolean }>).detail;
    if (!detail?.patch) return;

    const url = absHref(detail.href ?? window.location.href);
    if (url === lastLLVNavigatedHref) {
      lastLLVNavigatedHref = null;
      return;
    }

    llvHandleParams(url);
  });
}
