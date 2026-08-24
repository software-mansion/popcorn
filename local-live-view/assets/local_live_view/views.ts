import type { Socket as PhoenixSocket } from "phoenix";
import type { LLVSocket } from "./types";
import type { PopcornClient } from "./index";
import { llvIdOf } from "./helpers";

interface ViewData {
  lastAssigns?: string | null;
}

export class Views {
  private socket: LLVSocket;
  private pop: PopcornClient;
  private data = new Map<string, ViewData>();

  constructor(socket: LLVSocket, pop: PopcornClient) {
    this.socket = socket;
    this.pop = pop;
  }

  async mount(pop_view_el: HTMLElement): Promise<void> {
    const llvId = llvIdOf(pop_view_el);
    if (this.data.has(llvId)) return;
    const assigns = pop_view_el.getAttribute("data-pop-assigns");
    const data: ViewData = { lastAssigns: assigns };
    this.data.set(llvId, data);
    void this.pop.call({ action: "url_changed", url: window.location.href });
    const result = await this.pop.call(
      {
        action: "create",
        id: llvId,
        view: pop_view_el.getAttribute("data-pop-view")!,
        mirror_id: pop_view_el.dataset.popMirrorId ?? null,
        assigns,
      },
      { suppressErrorLog: true },
    );
    if (!result.ok) {
      console.error("LLV failed to create view", llvId, result.error);
      if (this.data.get(llvId) === data) this.data.delete(llvId);
      return;
    }
    // View unmounted while the create was in flight
    if (this.data.get(llvId) !== data) return;
    // HTML element removed while the create was in flight.
    // This covers host-less pages where there's no hook
    // that would handle the removal.
    if (!pop_view_el.isConnected) {
      this.data.delete(llvId);
      void this.pop.call({ action: "destroy", id: llvId });
      return;
    }
    const { html } = result.data as { html: string };
    // The rendered container (a sticky root carrying the canonical id and
    // the LV-signed session token) replaces the host-rendered placeholder
    // inside the mount point. The mount point is phx-update="ignore", so
    // host patches never descend to this subtree — the swapped-in
    // container survives. Join it through the patched newRootView, which
    // routes its channel to the popcorn socket.
    const root = pop_view_el.querySelector<HTMLElement>("[data-pop-root]");
    if (!root) {
      console.error("LLV: mount point has no [data-pop-root] element", llvId);
      return;
    }
    this.socket.newRootView(this.installContainer(root, html)).join();
  }

  // Stop a removed view's runtime process and drop it from the registry
  // (the delete gates the destroy: only ids with a create sent have an
  // entry — and mount only runs with the runtime up, so the destroy can be
  // sent unconditionally).
  unmount(pop_view_el: HTMLElement): void {
    const llvId = llvIdOf(pop_view_el);
    if (this.data.delete(llvId)) void this.pop.call({ action: "destroy", id: llvId });
    // The client View is deliberately NOT destroyed here — stock discard
    // handling covers every removal path: host patches discard the root
    // directly, and on live navigation (replaceMain moves sticky elements
    // into the new main) the new main's join patch discards the moved
    // husk; both run destroyViewByEl, whose View teardown sends the
    // channel leave. Destroying it ourselves BREAKS that: View.destroy
    // marks the element destroyed, DOMPatch's getNodeKey then returns
    // null for it, and the keyless husk gets positionally paired with the
    // new page's first content div — which the sticky guard in
    // onBeforeElUpdated silently swallows, leaving the new page blank
    // (verified against LV 1.1.31, kanban live-nav e2e).
  }

  // LLV views become LiveView roots at runtime: the WASM dispatcher renders
  // a sticky-root container (with a genuine data-phx-session token) for
  // each "create", and mount() swaps it in for the root placeholder
  // before joining it. From then on LiveView's stock machinery treats it as
  // a regular sticky root: element→view resolution (closestViewEl /
  // getViewByEl) scopes events to it, and stock discard handling destroys
  // it when its DOM goes away (see the note in unmount()).
  //
  // The one seam left is which socket a view's channel lives on. newRootView
  // is patched so that views created for [data-pop-root] containers get
  // their channel on the popcorn socket — the join and every subsequent
  // frame then run through PopcornTransport into the WASM dispatcher.
  // Everything else (join lifecycle, diff application, hooks inside the
  // view) is stock.
  //
  // mount() adopts containers explicitly (newRootView + join after
  // inserting the container), but the patch must cover more than that
  // call: joinRootViews also runs on stock paths — socket (re)connect,
  // phxChildAdded after host patches move sticky elements, replaceMain's
  // sticky preservation — and getRootById only dedupes elements whose View
  // is alive. A container whose View was destroyed in place (e.g.
  // join attempts exhausted) gets re-adopted by those scans; unpatched, its
  // channel would land on the real websocket. This patch is the single
  // choke point keeping every adoption of an LLV container on the popcorn
  // socket.
  patchAdoption(popcornSocket: PhoenixSocket): void {
    const origNewRootView = this.socket.newRootView.bind(this.socket);

    this.socket.newRootView = (el, flash, liveReferer) => {
      const view = origNewRootView(el, flash, liveReferer);
      if (el.matches?.("[data-pop-root]")) {
        // Replace the channel the View constructor opened on the real socket
        // (never joined — join() runs after newRootView returns), keeping the
        // View's own join-params closure: it reads the session token, static
        // and sticky flags off the element on every (re)join, and the
        // dispatcher forwards that payload verbatim to the channel. Remount
        // freshness needs nothing extra here: mounts read the current URL
        // and assigns from the dispatcher's ETS cache.
        const params = (view.channel as unknown as { params: () => Record<string, unknown> })
          .params;
        view.channel = popcornSocket.channel(`lv:${el.id}`, params);
      }
      return view;
    };
  }

  // Forward the mount point's data-pop-assigns to the runtime when it
  // differs from the last value sent (the create snapshot or a previous
  // update). The baseline lives in ViewData rather than on the hook
  // instance so it survives hook re-mounts: when the host's connected
  // render replaces a dead-rendered mount point the startup scan already
  // mounted from, the fresh hook's mounted() must still detect the
  // difference. An entry only exists
  // after mount ran, so the runtime is up whenever this forwards; a pre-boot
  // call finds no entry and is a no-op (the create snapshots the
  // then-current assigns anyway).
  syncAssigns(pop_view_el: HTMLElement): void {
    const llvId = llvIdOf(pop_view_el);
    const data = this.data.get(llvId);
    if (!data) return;
    const assigns = pop_view_el.getAttribute("data-pop-assigns");
    if (assigns === null || assigns === data.lastAssigns) return;
    data.lastAssigns = assigns;
    // A dedicated action, not an opaque dispatch: the dispatcher caches the
    // value (latest-wins, ETS) where any remount reads it before its
    // initial render, and forwards it to the live channel.
    void this.pop.call({ action: "update_assigns", id: llvId, assigns });
  }

  // Swap the host-rendered placeholder for the rendered container, and hand
  // the installed element back. The host never restores its template
  // children (the mount point is phx-update="ignore"), so the swap is
  // permanent for this incarnation; a remount of the wrapper renders a
  // fresh placeholder.
  private installContainer(root: HTMLElement, html: string): HTMLElement {
    const template = document.createElement("template");
    template.innerHTML = html;
    const rendered = template.content.firstElementChild as HTMLElement;
    root.replaceWith(rendered);
    return rendered;
  }
}
