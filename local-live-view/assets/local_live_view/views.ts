import type { LLVSocket } from "./types";
import type { PopcornClient } from "./index";
import { llvIdOf } from "./helpers";
import type { PopcornTransports } from "./transport";

interface ViewData {
  lastAssigns?: string | null;
  // The view's own LiveSocket, created once its container is installed.
  socket?: LLVSocket;
}

export class Views {
  private hostSocket: LLVSocket;
  private pop: PopcornClient;
  private transports: PopcornTransports;
  private data = new Map<string, ViewData>();

  constructor(hostSocket: LLVSocket, pop: PopcornClient, transports: PopcornTransports) {
    this.hostSocket = hostSocket;
    this.pop = pop;
    this.transports = transports;
  }

  // The LiveSocket owning the element: the view's own socket for elements
  // inside a mounted container, the host's otherwise.
  socketFor(el: Element): LLVSocket {
    const rootEl = el.closest("[data-pop-root]");
    const socket = rootEl && this.data.get(rootEl.id)?.socket;
    return socket || this.hostSocket;
  }

  socketById(llvId: string): LLVSocket | undefined {
    return this.data.get(llvId)?.socket;
  }

  mountedIds(): string[] {
    return Array.from(this.data.keys());
  }

  async mount(pop_view_el: HTMLElement): Promise<void> {
    const llvId = llvIdOf(pop_view_el);
    if (this.data.has(llvId)) return;
    const assigns = pop_view_el.getAttribute("data-pop-assigns");
    const data: ViewData = { lastAssigns: assigns };
    this.data.set(llvId, data);
    this.pop.call({ action: "url_changed", url: window.location.href });
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
      this.pop.call({ action: "destroy", id: llvId });
      return;
    }
    const { html } = result.data as { html: string };
    const slot = pop_view_el.querySelector<HTMLElement>("[data-pop-slot]");
    if (!slot) {
      console.error("LLV: mount point has no [data-pop-slot] element", llvId);
      return;
    }
    // The view's own LiveSocket, embedded in the slot: it places the locally
    // rendered container there and joins it through the fully stock path.
    data.socket = this.transports.newSocket(llvId);
    data.socket.embed(slot, () => html);
  }

  unmount(pop_view_el: HTMLElement): void {
    const llvId = llvIdOf(pop_view_el);
    const data = this.data.get(llvId);
    if (!data) return;
    this.data.delete(llvId);
    // Stock goodbye: the view's socket leaves its channel (phx_leave makes
    // the channel process exit itself with {:shutdown, :left} — no
    // server-side kill needed), runs the hooks' destroyed callbacks,
    // disconnects and releases its window listeners. The destroy action
    // then only cleans the dispatcher's registry/ETS (and its epoch guard
    // reaps a join still in flight — the one case where no client exists
    // to leave).
    data.socket?.destroy();
    this.pop.call({ action: "destroy", id: llvId });
  }

  syncAssigns(pop_view_el: HTMLElement): void {
    const llvId = llvIdOf(pop_view_el);
    const data = this.data.get(llvId);
    if (!data) return;
    const assigns = pop_view_el.getAttribute("data-pop-assigns");
    if (assigns === null || assigns === data.lastAssigns) return;
    data.lastAssigns = assigns;
    this.pop.call({ action: "update_assigns", id: llvId, assigns });
  }
}
