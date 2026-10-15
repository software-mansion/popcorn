import type { Channel } from "phoenix";
import type { LiveSocketInstanceInterface } from "phoenix_live_view";

// --- Public API ---

export interface LLVConfig {
  /**
   * URL of the directory with the Wasm runtime built by `mix llv.build`,
   * resolved against the URL of the script. Defaults to `"popcorn/"`.
   */
  assetsUrl?: string;
  /** Receives messages sent from the Wasm runtime with `Popcorn.Wasm.send/1` */
  eventHandler?: (event: unknown) => void;
}

// --- Wasm runtime events ---

/** Events sent by LocalLiveView.Dispatcher.send_to_js/2. Payloads are JSON-encoded. */
export type LLVEvent =
  | { __llv__: "transport_push"; frame: string }
  | { __llv__: "mirror_sync"; mirror_id: string; payload: string }
  | { __llv__: "push_server_event"; id: string; event: string; payload: string }
  | { __llv__: "navigate"; href: string; replace: boolean };

// --- Internal Phoenix types ---

/** A raw Phoenix channel frame as it crosses the (fake) transport. */
export interface TransportFrame {
  topic: string;
  event: string;
  payload: unknown;
  ref: string | null;
  join_ref: string | null;
}

export interface PointerData {
  clientX: number;
  clientY: number;
  pageX: number;
  pageY: number;
  screenX: number;
  screenY: number;
  movementX: number;
  movementY: number;
  button: number;
  buttons: number;
  altKey: boolean;
  ctrlKey: boolean;
  metaKey: boolean;
  shiftKey: boolean;
  rect: { top: number; left: number; width: number; height: number };
}

export interface LLVServerEventDetail {
  view: string;
  payload: unknown;
}

export interface LLVView {
  el: HTMLElement;
  channel: Channel;
  join(): void;
  addHook: (el: Element) => unknown;
  destroy?: (callback?: () => void) => void;
}

/**
 * A mounted LocalLiveViewEventBus hook instance: the host-side channel used
 * by __llvPushServer.
 */
export interface EventBusHook {
  el: HTMLElement;
  pushEvent(event: string, payload: Record<string, unknown>): Promise<unknown>;
  js(): { patch(href: string, opts?: { replace?: boolean }): void };
}

/**
 * LiveSocket members missing from LV's published LiveSocketInstanceInterface,
 * which LLV accesses via type-cast. All are private API.
 */
interface PhxLiveSocketInternals {
  newRootView(el: HTMLElement, ...rest: unknown[]): LLVView;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  hooks: Record<string, any>;
  debounce(el: Element, event: Event, eventType: string, callback: () => void): unknown;
  bindForms(): void;
}

/** Public LiveSocket interface extended with Phoenix internals accessed by LLV. */
export type LLVSocket = LiveSocketInstanceInterface & PhxLiveSocketInternals;
