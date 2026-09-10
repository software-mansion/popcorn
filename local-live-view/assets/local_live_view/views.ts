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
    const root = pop_view_el.querySelector<HTMLElement>("[data-pop-root]");
    if (!root) {
      console.error("LLV: mount point has no [data-pop-root] element", llvId);
      return;
    }
    this.socket.newRootView(this.installContainer(root, html)).join();
  }

  unmount(pop_view_el: HTMLElement): void {
    const llvId = llvIdOf(pop_view_el);
    if (this.data.delete(llvId)) this.pop.call({ action: "destroy", id: llvId });
  }

  // Replace the view's channel with the PopcornSocket channel
  // for the local views.
  // This relies on LV's private API and the fact that LV
  // opens the channel before it calls newRootView and joins it
  // afterwards.
  patchAdoption(popcornSocket: PhoenixSocket): void {
    const origNewRootView = this.socket.newRootView.bind(this.socket);

    this.socket.newRootView = (...args) => {
      const [el] = args;
      const view = origNewRootView(...args);
      if (el.matches?.("[data-pop-root]")) {
        const params = (view.channel as unknown as { params: () => Record<string, unknown> })
          .params;
        view.channel = popcornSocket.channel(`lv:${el.id}`, params);
      }
      return view;
    };
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

  // Replaces the host-rendered placeholder with the locally
  // rendered container
  private installContainer(root: HTMLElement, html: string): HTMLElement {
    const template = document.createElement("template");
    template.innerHTML = html;
    const rendered = template.content.firstElementChild as HTMLElement;
    root.replaceWith(rendered);
    return rendered;
  }
}
