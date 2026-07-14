import type { Popcorn } from "@swmansion/popcorn";

/** Timeout applied to every Popcorn call LLV makes. */
const DEFAULT_CALL_TIMEOUT_MS = 10_000;

type CallResult = Awaited<ReturnType<Popcorn["call"]>>;

interface CreateArgs {
  id: string;
  view: string | null;
  url: string;
  urlParams: Record<string, string>;
  assigns: Record<string, unknown>;
}

/**
 * Thin wrapper around the Popcorn runtime. Owns the single call timeout, the
 * action vocabulary spoken to the Elixir side, and fire-and-forget error
 * logging — so the rest of LLV never touches raw action strings or timeouts.
 *
 * The runtime boots asynchronously: the client is created up front and handed
 * to handlers that only invoke it at runtime (after boot). `attach` binds the
 * booted runtime once ready, replacing the old `{ current: Popcorn }` box.
 */
export class PopcornClient {
  private popcorn: Popcorn | null = null;

  /** Whether the runtime has finished booting and calls can be made. */
  get ready(): boolean {
    return this.popcorn !== null;
  }

  /** Bind the booted runtime. Called once after `Popcorn.init()`. */
  attach(popcorn: Popcorn): void {
    this.popcorn = popcorn;
  }

  private call(args: Record<string, unknown>): Promise<CallResult> {
    if (!this.popcorn) {
      return Promise.reject(new Error("LLV: PopcornClient used before runtime was ready"));
    }
    return this.popcorn.call(args, { timeoutMs: DEFAULT_CALL_TIMEOUT_MS });
  }

  // Fire-and-forget: log on error, callers don't await the outcome.
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

  event(id: string, payload: Record<string, unknown>): void {
    this.fire("event", { action: "event", id, payload });
  }

  push(id: string, event: string, payload: Record<string, unknown>): Promise<CallResult> {
    return this.call({ action: "push", id, payload: { event, payload } });
  }
}
