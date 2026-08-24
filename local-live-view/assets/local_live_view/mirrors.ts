import type { Channel, Socket as PhoenixSocket } from "phoenix";
import type { PopcornClient } from "./index";
import { llvIdOf } from "./helpers";

export class Mirrors {
  private socketClass: () => typeof PhoenixSocket;
  private pop: PopcornClient;
  private channels: Record<string, Channel> = {};
  private socket: PhoenixSocket | undefined;

  constructor(socketClass: () => typeof PhoenixSocket, pop: PopcornClient) {
    this.socketClass = socketClass;
    this.pop = pop;
  }

  installSync(): void {
    window.__llvSync = (mirrorId: string, eventName: string, payload: Record<string, unknown>) => {
      this.channels[mirrorId]?.push(eventName, payload);
    };
  }

  ensureChannel(pop_view_el: HTMLElement): void {
    const llvId = llvIdOf(pop_view_el);
    const mirrorId = pop_view_el.dataset.popMirrorId;
    const token = pop_view_el.dataset.popMirrorToken;
    if (!mirrorId || this.channels[mirrorId] || !token) return;

    const channel = this.connectedSocket().channel(`llv:${mirrorId}`, {
      view: pop_view_el.dataset.popView,
      token,
    });
    this.channels[mirrorId] = channel;
    let rejoin = false;
    channel
      .join()
      .receive("ok", () => {
        if (rejoin) {
          void this.pop.call({
            action: "dispatch_to_view",
            id: llvId,
            payload: { action: "mirror_reconnected" },
          });
        }
        rejoin = true;
      })
      .receive("error", (err: unknown) => console.error("LLV channel join error", err));
  }

  private connectedSocket(): PhoenixSocket {
    if (this.socket) return this.socket;

    const Socket = this.socketClass();
    const csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content");
    this.socket = new Socket("/llv_socket", {
      params: { _csrf_token: csrfToken },
    });
    this.socket.connect();
    return this.socket;
  }
}
