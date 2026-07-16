import type { Socket as PhoenixSocket } from "phoenix";
import type { LiveSocketInstanceInterface } from "phoenix_live_view";

// --- Public API ---

export interface LLVConfig {
  /** Phoenix Socket class — required when using mirror channels */
  Socket?: typeof PhoenixSocket;
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

export type RefGenerator = (opts: { payload: EventPayload }) => [unknown, Element[], object];

export interface EventPayload {
  cid?: number;
  event?: string;
  value?: unknown;
  type?: string;
  [key: string]: unknown;
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

export const LLV_DEFAULT_TARGET = "__llv_default__";
export const LLV_SERVER_TARGET = "__llv_server__";
export const LLV_TARGET_SEP = "\x1f";

// Phoenix tags every real LiveView root element with data-phx-session; this is
// the selector phoenix_live_view uses internally (PHX_VIEW_SELECTOR).
export const PHX_VIEW_SELECTOR = "[data-phx-session]";

/** Phoenix View properties and methods that LLV reads or calls but does not replace. */
interface PhxView {
  el: HTMLElement;
  liveSocket: LLVSocket;
  joinCount: number;
  joinCallback: (onDone?: () => void) => void;
  pushEvent(
    type: string,
    el: Element,
    targetCtx: null,
    phxEvent: string,
    meta: PointerData,
    opts: object,
  ): void;
  showLoader(timeout: number): void;
  onJoin(resp: { rendered: RenderedDiff }): void;
  update(diff: RenderedDiff, events: unknown[]): boolean;
  undoRefs(ref: unknown, event: string): void;
  destroy?: (callback?: () => void) => void;
}

/**
 * Phoenix View methods that LLV replaces via monkey-patching on the object
 * returned by socket.newRootView(). Property (arrow) syntax is required so
 * TypeScript allows reassignment on the concrete instance.
 */
interface PhxViewPatchable {
  withinTargets: (
    phxTarget: unknown,
    callback: (view: LLVView, ctx: null) => void,
    dom?: unknown,
  ) => void;
  addHook: (el: Element) => unknown;
  isConnected: () => boolean;
  join: (callback?: (count: number, onDone?: () => void) => void) => void;
  pushWithReply: (
    refGenerator: RefGenerator | null,
    event: string,
    payload: EventPayload,
  ) => Promise<{ resp: object; reply: null; ref: unknown }>;
  maybePushComponentsDestroyed: (destroyedCIDs: number[]) => void;
  bindChannel: () => void;
}

/** Phoenix View shape after LLV patches it in view_setup. */
export type LLVView = PhxView & PhxViewPatchable;

/** Registry of the fake Phoenix views LLV mounts, keyed by element id. */
export type ViewRegistry = Map<string, LLVView>;

/** Private phoenix_live_view LiveSocket API that LLV accesses via type-cast. */
interface PhxLiveSocketInternals {
  newRootView(el: HTMLElement, flash?: unknown, liveReferer?: unknown): LLVView;
  requestDOMUpdate(callback: () => void): void;
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
  loaderTimeout: number;
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
