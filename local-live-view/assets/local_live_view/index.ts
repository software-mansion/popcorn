import { Popcorn } from "@swmansion/popcorn";
import type { Socket as PhoenixSocket } from "phoenix";
import type { Hook, LiveSocketInstanceInterface } from "phoenix_live_view";
import type {
  EventBusHook,
  LLVConfig,
  LLVSocket,
  LLVServerMessageDetail,
  TransportFrame,
} from "./types";
import { createPopcornSocket, type PopcornLink } from "./transport";
import { registerNavigationHandlers } from "./navigation";
import { registerCustomEventBindings } from "./events";
import { resolveLlvId } from "./helpers";
import { Views } from "./views";
import { Mirrors } from "./mirrors";

export type { LLVConfig };
export type { PopcornClient };

const DEFAULT_CALL_TIMEOUT_MS = 10_000;

type CallResult = Awaited<ReturnType<Popcorn["call"]>>;

class PopcornClient {
  private popcorn: Popcorn | null = null;

  get ready(): boolean {
    return this.popcorn !== null;
  }

  attach(popcorn: Popcorn): void {
    this.popcorn = popcorn;
  }

  /**
   * Sends an action to the dispatcher, return's dispatcher's reply.
   * By default, logs when an error reply is received. Disable with
   * suppressErrorLog: true.
   */
  call(
    message: { action: string; [key: string]: unknown },
    opts?: { suppressErrorLog?: boolean },
  ): Promise<CallResult> {
    const called = this.popcorn
      ? this.popcorn.call(message, { timeoutMs: DEFAULT_CALL_TIMEOUT_MS })
      : Promise.reject(new Error("LLV: popcorn call before runtime was ready"));
    return called
      .catch((err: unknown): CallResult => ({
        ok: false,
        error: err instanceof Error ? err : new Error(String(err)),
        durationMs: 0,
      }))
      .then((result) => {
        if (!result.ok && !opts?.suppressErrorLog) {
          console.error(`LLV ${message.action} error`, result.error);
        }
        return result;
      });
  }
}

export class LLVEngine {
  private socket: LLVSocket;
  private config: LLVConfig;
  private pop = new PopcornClient();
  private views: Views;
  private mirrors: Mirrors;
  private bufferedServerMessages: LLVServerMessageDetail[] = [];
  // llvId -> mounted LocalLiveViewEventBus hook: the host-side channel used
  // by __llvPushServer. Host lifecycle, deliberately not part of Views: the
  // bus mounts before the view's create (the host joins while WASM boots)
  // and is replaced when the host remounts.
  private eventBusHooks = new Map<string, EventBusHook>();
  private popcornLink!: PopcornLink;
  private connectPromise: Promise<void> | null = null;

  private constructor(socket: LLVSocket, config: LLVConfig) {
    this.socket = socket;
    this.config = config;
    this.views = new Views(this.socket, this.pop);
    this.mirrors = new Mirrors(() => this.socketClass(), this.pop);
  }

  /**
   * Creates an LLVEngine instance. Must be called before `livesocket.connect()`.
   * @param liveSocket - The phoenix_live_view LiveSocket instance.
   * @param config - Optional LLV configuration.
   */
  static create(liveSocket: LiveSocketInstanceInterface, config: LLVConfig = {}): LLVEngine {
    const engine = new LLVEngine(liveSocket as LLVSocket, config);
    engine.registerServerMessageListener();
    registerNavigationHandlers(engine.socket, engine.pop, engine.config);
    engine.registerHooks();
    engine.bindFormsIfHostless();
    engine.connectPopcornSocket();
    engine.views.patchAdoption(engine.popcornLink.socket);
    return engine;
  }

  /**
   * Boots the WASM runtime and mounts every LocalLiveView on the page.
   * Typically called right after `liveSocket.connect()`. Idempotent —
   * repeated calls are no-ops.
   */
  connect(): void {
    // The stored promise exists only to make repeated calls no-ops; a boot
    // failure is logged here because no caller holds the promise.
    this.connectPromise ??= this.doConnect().catch((err: unknown) => {
      console.error("LLV: connect failed", err);
    });
  }

  private async doConnect(): Promise<void> {
    await this.bootPopcorn();

    this.mirrors.installSync();
    this.exposeGlobals();
    registerCustomEventBindings(this.socket);

    await this.scanAndMount();
    this.flushBufferedServerMessages();
  }

  // The app's Phoenix Socket class, recovered from the live instance the
  // LiveSocket already holds — same class, same version, same module as the
  // one LiveView runs on, with nothing to configure.
  private socketClass(): typeof PhoenixSocket {
    return this.socket.getSocket().constructor as typeof PhoenixSocket;
  }

  private connectPopcornSocket(): void {
    this.popcornLink = createPopcornSocket(this.socketClass(), this.pop);
  }

  private mountView(pop_view_el: HTMLElement): Promise<void> {
    this.mirrors.ensureChannel(pop_view_el);
    return this.views.mount(pop_view_el);
  }

  private registerServerMessageListener(): void {
    window.addEventListener("phx:llv_server_message", (e: Event) => {
      const detail = (e as CustomEvent<LLVServerMessageDetail>).detail;
      if (!this.pop.ready) {
        this.bufferedServerMessages.push(detail);
        return;
      }
      this.sendServerMessage(detail);
    });
  }

  private sendServerMessage(detail: LLVServerMessageDetail): void {
    void this.pop.call({
      action: "dispatch_to_view",
      id: resolveLlvId(detail.view),
      payload: { action: "server_message", params: detail.payload },
    });
  }

  // Hooks that manage views rendered inside a host LiveView (other views are
  // mounted by the startup scan, which also catches hooks that fired before
  // Popcorn was ready) and the per-view event bus.
  private registerHooks(): void {
    const pop = this.pop;
    const views = this.views;
    const mountView = (el: HTMLElement) => this.mountView(el);
    this.socket.hooks.LocalLiveView = {
      mounted() {
        if (pop.ready) void mountView(this.el);
        // Sync assigns in case view was already mounted by the startup scan
        // (mountView is a noop) but the assigns changed.
        views.syncAssigns(this.el);
      },
      updated() {
        views.syncAssigns(this.el);
      },
      destroyed() {
        // The element is already detached; its subtree is still intact.
        views.unmount(this.el);
      },
    } satisfies Hook;

    // Hook for sending events from client to server via `LocalLiveView.push_server_event`
    // Sending an event via a hook freezes the element the hook binds to along with all
    // its descendants until the event is internally ACKed by LiveView, thus we use
    // a dedicated, empty div for that.
    const eventBusHooks = this.eventBusHooks;
    this.socket.hooks.LocalLiveViewEventBus = {
      mounted() {
        const llvId = this.el.getAttribute("data-llv-event-bus-for");
        if (llvId) eventBusHooks.set(llvId, this as unknown as EventBusHook);
      },
      destroyed() {
        const llvId = this.el.getAttribute("data-llv-event-bus-for");
        if (llvId && eventBusHooks.get(llvId) === (this as unknown as EventBusHook)) {
          eventBusHooks.delete(llvId);
        }
      },
    } satisfies Hook;
  }

  // Pages with only LocalLiveViews (no server-side LiveView) connect in "dead"
  // mode, which skips bindForms() — making phx-submit / phx-change no-ops on
  // any LLV. Wire them up manually when no real LiveView is on the page.
  private bindFormsIfHostless(): void {
    if (!document.querySelector("[data-phx-session]:not([data-pop-root])")) {
      this.socket.bindForms();
    }
  }

  private async bootPopcorn(): Promise<void> {
    const popcorn = await Popcorn.init({
      debug: this.config.debug ?? false,
      bundlePaths: this.config.bundlePaths ?? ["wasm/bundle.avm"],
    });
    this.pop.attach(popcorn);

    if (this.config.eventHandler) {
      popcorn.onMessage(this.config.eventHandler);
    }
  }

  private exposeGlobals(): void {
    window.__llvPopcornTransportPush = (frame: TransportFrame) => {
      this.popcornLink.inject(frame);
    };

    window.__llvPushServer = (llvId: string, event: string, payload: Record<string, unknown>) => {
      const pushError = () =>
        void this.pop.call({
          action: "dispatch_to_view",
          id: llvId,
          payload: { action: "push_error", event, params: payload },
        });
      const eventBus = this.eventBusHooks.get(llvId);
      if (!eventBus) {
        console.error("LLV push_server_event: no host event bus for view", llvId);
        pushError();
        return;
      }
      eventBus.pushEvent(event, payload).catch((err: unknown) => {
        console.error("LLV push_server_event failed", err);
        pushError();
      });
    };
  }

  // Startup scan: mount every [data-pop-view] present now that Popcorn is up.
  // This is the mount path for host-less pages (no hooks fire there) and the
  // catch-up for hooks that fired before Popcorn was ready (Views.mount
  // dedupes on its registry).
  private async scanAndMount(): Promise<void> {
    const pop_view_els = Array.from(document.querySelectorAll<HTMLElement>("[data-pop-view]"));
    await Promise.all(pop_view_els.map((el) => this.mountView(el)));
  }

  // Flush any server messages that arrived during Popcorn initialization.
  private flushBufferedServerMessages(): void {
    for (const detail of this.bufferedServerMessages) {
      this.sendServerMessage(detail);
    }
    this.bufferedServerMessages = [];
  }

  /**
   * Pushes an event into a local view from external JavaScript.
   *
   * @param viewId - The view name (e.g. `"ThermostatLive"`) or element id.
   * @param event - The event name dispatched to `handle_event/3`.
   * @param payload - Optional params map for the callback.
   */
  async pushEvent(
    viewId: string,
    event: string,
    payload: Record<string, unknown> = {},
  ): Promise<void> {
    const result = await this.pop.call(
      {
        action: "dispatch_to_view",
        id: resolveLlvId(viewId),
        payload: { action: "push_event", event, params: payload },
        queue: "unless_dead",
      },
      { suppressErrorLog: true },
    );

    if (!result.ok) {
      throw new Error(`LLV pushEvent: ${String(result.error)}`);
    }
  }
}
