import { Popcorn } from "@swmansion/popcorn";
import type { Channel, Socket as PhoenixSocket } from "phoenix";
import type { Hook, LiveSocketInstanceInterface } from "phoenix_live_view";
import type {
  EventBusHook,
  LLVConfig,
  LLVSocket,
  LLVServerMessageDetail,
  LLVView,
  RenderedDiff,
  ViewRegistry,
} from "./types";
import { setupFakeView } from "./view_setup";
import { createPopcornSocket, type PopcornLink } from "./transport";
import { registerNavigationHandlers } from "./navigation";
import { registerCustomEventBindings } from "./events";
import { sendServerMessage, parseAssigns, resolveLlvId } from "./helpers";

export type { LLVConfig };
export type { PopcornClient };

const DEFAULT_CALL_TIMEOUT_MS = 10_000;

type CallResult = Awaited<ReturnType<Popcorn["call"]>>;

interface CreateArgs {
  id: string;
  view: string | null;
  url: string;
  urlParams: Record<string, string>;
  assigns: Record<string, unknown>;
}

class PopcornClient {
  private popcorn: Popcorn | null = null;

  get ready(): boolean {
    return this.popcorn !== null;
  }

  attach(popcorn: Popcorn): void {
    this.popcorn = popcorn;
  }

  private call(args: Record<string, unknown>): Promise<CallResult> {
    if (!this.popcorn) {
      return Promise.reject(new Error("LLV: PopcornClient used before runtime was ready"));
    }
    return this.popcorn.call(args, { timeoutMs: DEFAULT_CALL_TIMEOUT_MS });
  }

  private fire(action: string, args: Record<string, unknown>): void {
    this.call(args).then(
      (result) => {
        if (!result.ok) console.error(`LLV ${action} error`, result.error);
      },
      (err) => console.error(`LLV ${action} error`, err),
    );
  }

  create({ id, view, url, urlParams, assigns }: CreateArgs): Promise<CallResult> {
    return this.call({
      action: "create",
      id,
      view,
      url,
      url_params: urlParams,
      assigns,
    });
  }

  destroy(id: string): void {
    this.fire("destroy", { action: "destroy", id, payload: {} });
  }

  reconnected(id: string): void {
    this.fire("reconnect sync", { action: "reconnected", id, payload: {} });
  }

  updateAssigns(id: string, assigns: Record<string, unknown>): void {
    this.fire("update assigns", { action: "update_assigns", id, assigns });
  }

  handleParams(id: string, params: Record<string, string>, url: string): void {
    this.fire("handle_params", { action: "handle_params", id, payload: { params, url } });
  }

  // A channel frame the view must answer: the view's process settles the
  // call promise (Popcorn.Wasm.resolve) once the frame is processed, so the
  // result carries the channel ack. The transport consumes it — no `fire`.
  transportFrame(id: string, event: string, payload: unknown): Promise<CallResult> {
    return this.call({ action: "transport_frame", id, event, payload });
  }

  serverMessage(id: string, payload: Record<string, unknown>): void {
    this.fire("server_message", { action: "server_message", id, payload });
  }

  // Report a failed local→server push back to the view's process so its
  // handle_push_error callback runs. `payload` is the map the view passed
  // to push_server_event.
  pushError(id: string, event: string, payload: Record<string, unknown>): void {
    this.fire("push_error report", { action: "push_error", id, payload: { event, payload } });
  }

  push(id: string, event: string, payload: Record<string, unknown>): Promise<CallResult> {
    return this.call({ action: "push", id, payload: { event, payload } });
  }
}

export class LLVEngine {
  private socket: LLVSocket;
  private config: LLVConfig;
  private pop = new PopcornClient();
  private views: ViewRegistry = new Map();
  private channels: Record<string, Channel> = {};
  private bufferedServerMessages: LLVServerMessageDetail[] = [];
  // llvId -> mounted LocalLiveViewEventBus hook: the host-side channel used
  // by __llvPushServer.
  private eventBusHooks = new Map<string, EventBusHook>();
  private popcornLink!: PopcornLink;

  private constructor(socket: LLVSocket, config: LLVConfig) {
    this.socket = socket;
    this.config = config;
  }

  /**
   * Initializes LLVEngine and connects the LiveSocket.
   *
   * @param liveSocket - The phoenix_live_view LiveSocket instance.
   * @param config - Optional LLV configuration.
   */
  static async create(
    liveSocket: LiveSocketInstanceInterface,
    config: LLVConfig = {},
  ): Promise<LLVEngine> {
    const engine = new LLVEngine(liveSocket as LLVSocket, config);

    // Synchronous registration must happen BEFORE any awaits (see
    // registerServerMessageListener / registerHooks comments).
    engine.registerServerMessageListener();
    registerNavigationHandlers(engine.socket, engine.views, engine.pop, engine.config);
    engine.registerHooks();
    engine.bindFormsIfHostless();
    engine.connectPopcornSocket();

    await engine.bootPopcorn();

    engine.setupMirrorChannels();
    engine.exposeGlobals();
    engine.patchOwner();
    registerCustomEventBindings(engine.socket);

    await engine.scanAndMount();
    engine.flushBufferedServerMessages();

    return engine;
  }

  // The dedicated never-networked socket the fake views' channels live on.
  // Connected before Popcorn boots: heartbeats are acked in place by the
  // transport, and no answered frame flows until a view mounts (post-boot).
  private connectPopcornSocket(): void {
    const SocketClass =
      this.config.Socket ?? (this.socket.getSocket().constructor as typeof PhoenixSocket);
    this.popcornLink = createPopcornSocket(SocketClass, this.pop);
  }

  // Start a view and wire it up.
  private async mountView(pop_view_el: HTMLElement): Promise<void> {
    const llvId = pop_view_el.id;
    if (this.views.has(llvId)) return;
    const result = await this.pop.create({
      id: llvId,
      view: pop_view_el.getAttribute("data-pop-view"),
      url: window.location.href,
      urlParams: Object.fromEntries(new URLSearchParams(window.location.search)),
      assigns: parseAssigns(pop_view_el.getAttribute("data-pop-assigns")),
    });
    // A rejected call resolves with ok: false (it does not throw). Bail on
    // failed or duplicate creates — wiring a fake view without a live
    // process would join a channel nobody answers.
    if (!result.ok) {
      console.error("LLV failed to create view", llvId, result.error);
      return;
    }
    const liveEl = document.getElementById(llvId);
    if (liveEl?.matches("[data-pop-view]")) {
      setupFakeView(this.socket, this.views, this.popcornLink.socket, liveEl);
    } else {
      this.pop.destroy(llvId);
    }
  }

  // Stop a view's runtime process and drop its fake view. Used by the hook when
  // the host LiveView removes the mount point.
  private unmountView(pop_view_el: HTMLElement): void {
    const llvId = pop_view_el.id;
    const view = this.views.get(llvId);
    if (!view) return;
    this.views.delete(llvId);
    view.destroy?.();
    this.pop.destroy(llvId);
  }

  // Register the server message listener immediately — BEFORE any awaits.
  // push_event("llv_server_message") from Phoenix LiveView fires during the initial
  // LiveView join, which happens before Popcorn finishes initializing. Without this,
  // the event is dispatched on window before our listener is registered and is lost.
  private registerServerMessageListener(): void {
    window.addEventListener("phx:llv_server_message", (e: Event) => {
      const detail = (e as CustomEvent<LLVServerMessageDetail>).detail;
      if (!this.pop.ready) {
        this.bufferedServerMessages.push(detail);
        return;
      }
      sendServerMessage(this.pop, detail);
    });
  }

  // Hooks that manage views rendered inside a host LiveView (other views are
  // mounted by the startup scan, which also catches hooks that fired before
  // Popcorn was ready) and the per-view event bus.
  private registerHooks(): void {
    const pop = this.pop;
    const mountView = (el: HTMLElement) => this.mountView(el);
    const unmountView = (el: HTMLElement) => this.unmountView(el);
    this.socket.hooks.LocalLiveView = {
      mounted() {
        this.llvLastAssigns = this.el.getAttribute("data-pop-assigns");
        if (pop.ready) mountView(this.el);
      },
      updated() {
        const raw = this.el.getAttribute("data-pop-assigns");
        if (raw === this.llvLastAssigns) return;
        this.llvLastAssigns = raw;
        // Not mounted yet (Popcorn still booting): the mount reads the current
        // assigns, so there's nothing to forward. Once mounted, the dispatcher
        // processes this after the mount (it's sent after, and calls are FIFO).
        if (!pop.ready) return;
        pop.updateAssigns(this.el.id, parseAssigns(raw));
      },
      destroyed() {
        unmountView(this.el);
      },
    } satisfies Hook;

    // Event bus for local→server pushes: a hidden sibling of each mount
    // point, owned by the HOST LiveView (see LocalLiveView.Component). Its
    // lifecycle IS the host resolution — LiveView mounts and destroys it
    // with the host view across reconnects and re-renders, so the registry
    // always holds a live hook and no host-element lookup can go stale.
    // __llvPushServer pushes through it via the documented hook pushEvent.
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
    if (!document.querySelector("[data-phx-session]")) {
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

  // Mirror channels: only created for views with a server-side Mirror module.
  private setupMirrorChannels(): void {
    const mirrorEls = document.querySelectorAll<HTMLElement>(
      "[data-pop-view][data-pop-mirror-token]",
    );
    if (mirrorEls.length === 0) return;

    const { Socket } = this.config;
    const csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content");

    if (!Socket) {
      throw new Error("LLV: config.Socket is required when using mirror channels");
    }

    const llvSocket = new Socket("/llv_socket", {
      params: { _csrf_token: csrfToken },
    });
    llvSocket.connect();

    mirrorEls.forEach((el) => {
      const llvId = el.id;
      const channel = llvSocket.channel(`llv:${llvId}`, {
        view: el.dataset.popView,
        token: el.dataset.popMirrorToken,
      });
      this.channels[llvId] = channel;

      channel
        .join()
        .receive("ok", () => {
          if (this.views.has(llvId)) {
            this.pop.reconnected(llvId);
          }
        })
        .receive("error", (err: unknown) => console.error("LLV channel join error", err));
    });

    window.__llvSync = (id: string, eventName: string, payload: Record<string, unknown>) => {
      const channel = this.channels[id];
      if (channel) {
        channel.push(eventName, payload);
      }
    };
  }

  private exposeGlobals(): void {
    // Out-of-band diffs (handle_info renders, send_update, push_error
    // rollbacks, server messages — anything not acking a push). Injected as
    // a channel "diff" frame, handled by the stock bindChannel handler
    // exactly like a server-pushed diff. A diff racing a just-unmounted
    // view is dropped.
    window.__popcornTransportReceive = (llvId: string, diff: RenderedDiff) => {
      if (!this.views.has(llvId)) return;
      this.popcornLink.inject({
        topic: `lv:${llvId}`,
        event: "diff",
        payload: diff,
        ref: null,
        join_ref: null,
      });
    };

    // __llvPushServer: sends an event to the host LiveView, called from a
    // local view's Elixir via LocalLiveView.push_server_event/3. Pushes
    // through the event bus hook's documented, promise-returning pushEvent —
    // it rejects when the host is disconnected, replies with an error, or
    // times out; no event bus means no host LiveView on the page. Failures
    // are reported back so the view's handle_push_error runs.
    window.__llvPushServer = (llvId: string, event: string, payload: Record<string, unknown>) => {
      const eventBus = this.eventBusHooks.get(llvId);
      if (!eventBus) {
        console.error("LLV push_server_event: no host event bus for view", llvId);
        this.pop.pushError(llvId, event, payload);
        return;
      }
      eventBus.pushEvent(event, payload).catch((err: unknown) => {
        console.error("LLV push_server_event failed", err);
        this.pop.pushError(llvId, event, payload);
      });
    };
  }

  // owner: route events from inside [data-pop-view] elements to our fake views.
  // We never set data-phx-session on LLV elements, so Phoenix's default closestViewEl()
  // would walk up to the parent LiveView and dispatch events there instead.
  private patchOwner(): void {
    const views = this.views;
    const origOwner = this.socket.owner.bind(this.socket);
    this.socket.owner = function (childEl: Element, callback?: (view: LLVView) => unknown) {
      const llvEl = childEl.closest("[data-pop-view]");
      const view = llvEl ? views.get(llvEl.id) : undefined;
      if (view) {
        return callback ? callback(view) : view;
      }
      return origOwner(childEl, callback);
    };
  }

  // Startup scan: mount every [data-pop-view] present now that Popcorn is up.
  // This is the mount path for host-less pages (no hooks fire there) and the
  // catch-up for hooks that fired before Popcorn was ready.
  // If a view is mounted twice (here and by the hook), the dispatcher
  // on the Elixir side rejects the second create.
  private async scanAndMount(): Promise<void> {
    const pop_view_els = Array.from(document.querySelectorAll<HTMLElement>("[data-pop-view]"));
    await Promise.all(pop_view_els.map((el) => this.mountView(el)));
  }

  // Flush any server messages that arrived during Popcorn initialization.
  private flushBufferedServerMessages(): void {
    for (const detail of this.bufferedServerMessages) {
      sendServerMessage(this.pop, detail);
    }
    this.bufferedServerMessages = [];
  }

  /**
   * Pushes an event into a LLVEngine from external JavaScript.
   *
   * @param viewId - The view name (e.g. `"ThermostatLive"`) or element id.
   * @param event - The event name to dispatch into the view's `handle_info/2`.
   * @param payload - Optional payload map passed alongside the event.
   */
  async pushEvent(
    viewId: string,
    event: string,
    payload: Record<string, unknown> = {},
  ): Promise<void> {
    const result = await this.pop.push(resolveLlvId(viewId), event, payload);

    if (!result.ok) {
      console.error(`LLV pushEvent error for view "${viewId}", event "${event}":`, result);
    }
  }
}
