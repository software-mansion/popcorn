import type { Channel } from "phoenix";
import type { LiveSocketInstanceInterface } from "phoenix_live_view";

// --- Public API ---

export interface LLVConfig {
  /** Paths to compiled WASM bundle files. Defaults to `["wasm/bundle.avm"]` */
  bundlePaths?: string[];
  /** Enable Popcorn debug logging */
  debug?: boolean;
  /** Callback for raw Popcorn messages */
  eventHandler?: (msg: unknown) => void;
  /**
   * Override LLV's default navigation handler.
   * Called instead of `liveSocket.historyPatch` when an LLV view calls `push_patch`.
   * Pass a custom function to take full control of navigation.
   */
  onNavigate?: (href: string, replace: boolean) => void;
}

// --- Internal Phoenix types ---

/** Opaque rendered diff payload delivered from WASM via structuredClone */
export type RenderedDiff = Record<string, unknown>;

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

export interface LLVServerMessageDetail {
  view: string;
  payload: unknown;
}

export interface LLVView {
  el: HTMLElement;
  channel: Channel;
  join(): void;
  pushEvent(
    type: string,
    el: Element,
    targetCtx: null,
    phxEvent: string,
    meta: PointerData,
    opts: object,
  ): void;
  addHook: (el: Element) => unknown;
  destroy?: (callback?: () => void) => void;
}

/** Registry of the fake Phoenix views LLV mounts, keyed by element id. */
export type ViewRegistry = Map<string, LLVView>;

/**
 * A mounted LocalLiveViewEventBus hook instance: the host-side channel used
 * by __llvPushServer.
 */
export interface EventBusHook {
  el: HTMLElement;
  pushEvent(event: string, payload: Record<string, unknown>): Promise<unknown>;
}

/** Private phoenix_live_view LiveSocket API that LLV accesses via type-cast. */
interface PhxLiveSocketInternals {
  newRootView(el: HTMLElement, flash?: unknown, liveReferer?: unknown): LLVView;
  isConnected(): boolean;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  hooks: Record<string, any>;
  owner(childEl: Element, callback?: (view: LLVView) => unknown): unknown;
  withinOwners(childEl: Element, callback: (view: LLVView) => void): void;
  debounce(el: Element, event: Event, eventType: string, callback: () => void): unknown;
  pushHistoryPatch(
    event: Event | { isTrusted: boolean; type: string },
    href: string,
    linkState: string,
    targetEl: Element | null,
  ): void;
  bindForms(): void;
}

/** Public LiveSocket interface extended with Phoenix internals accessed by LLV. */
export type LLVSocket = LiveSocketInstanceInterface & PhxLiveSocketInternals;

declare global {
  interface Window {
    __popcornTransportReceive: (llvId: string, diff: RenderedDiff) => void;
    __llvSync?: (id: string, eventName: string, payload: Record<string, unknown>) => void;
    __llvPushServer?: (llvId: string, event: string, payload: Record<string, unknown>) => void;
  }
}
