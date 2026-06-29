import { PopcornError, err, type Result } from "./errors";
import {
  readWorkerEvent,
  serializeSendPayload,
  toVm,
  type PopcornEvent,
  type SendCompletionPayload,
} from "./events";
import type { AnyValue, BeamBootOptions, OtpErrorPayload } from "./types";
import { check, unreachable } from "./utils";

export type PopcornOpts = {
  beam: Pick<BeamBootOptions, "assetsUrl" | "searchPaths" | "extraArgs">;
  timeoutsMs?: {
    boot?: number;
    send?: number;
  };
  onError?: (event: OtpErrorPayload) => void;
  workerUrl?: string | URL;
};

export type PopcornSendOpts = {
  meta?: AnyValue;
};

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
  send: 5_000,
};

const LOG_PREFIX = "[Popcorn]";

type VmExitReason =
  | { reason: "deinit" }
  | { reason: "abort"; data: string }
  | { reason: "error"; data: string }
  | { reason: "exit"; data: number };

type PopcornState =
  | { status: "created" }
  | { status: "booting" }
  | { status: "booted" }
  | { status: "closed"; error: PopcornError<"vm:exited"> };
type PendingSend = (result: Result<null>) => void;

export class Popcorn {
  private vmWorker!: Worker;
  private state: PopcornState = { status: "created" };
  private readonly opts: PopcornOpts;
  private requestSeq = 0;
  private settleBoot: ((result: Result<Popcorn>) => void) | null = null;
  private readonly eventHandlers = new Set<(event: PopcornEvent) => void>();
  private readonly pendingSends = new Map<string, PendingSend>();
  private readonly onWorkerMessage = (event: MessageEvent<unknown>) => {
    const data = readWorkerEvent(event.data);
    check(data !== null);

    switch (data.type) {
      case "popcorn:boot-end":
      case "popcorn:boot-fail":
        return;
      case "otp:message":
        this.emit(data.payload);
        return;
      case "otp:run_js":
        this.runJs(data.payload.code);
        return;
      case "otp:stdout":
        console.log(`${LOG_PREFIX} stdout:`, data.payload);
        return;
      case "otp:stderr":
        console.error(`${LOG_PREFIX} stderr:`, data.payload);
        return;
      case "otp:error":
        this.handleOtpError(data.payload);
        return;
      case "popcorn:send-end": {
        this.completeSend(data.payload);
        return;
      }
      default:
        unreachable();
    }
  };

  public constructor(opts: PopcornOpts) {
    this.opts = opts;
    this.spawnWorker();
  }

  private spawnWorker(): void {
    const defaultWorkerUrl = new URL("./worker.mjs", import.meta.url);
    const workerUrl = this.opts.workerUrl ?? defaultWorkerUrl;
    this.vmWorker = new Worker(workerUrl, { type: "module" });
    this.vmWorker.addEventListener("message", this.onWorkerMessage);
  }

  public static async init(opts: PopcornOpts): Promise<Result<Popcorn>> {
    if (!canEval()) {
      return { ok: false, error: err("runtime:eval-unavailable", {}) };
    }

    const popcorn = new Popcorn(opts);
    const result = await popcorn.boot();

    if (!result.ok) {
      return result;
    }

    return { ok: true, data: popcorn };
  }

  public async boot(): Promise<Result<Popcorn>> {
    if (this.state.status === "booted") {
      return { ok: true, data: this };
    }

    if (this.state.status === "booting") {
      // TODO(jgonet): make it easier to construct check() errors without throwing
      const error = err("internal:check", {
        detail: "Boot already in progress",
      });
      return { ok: false, error };
    }

    const reboot = this.state.status === "closed";
    if (reboot) {
      this.spawnWorker();
    }

    this.state = { status: "booting" };

    return await new Promise<Result<Popcorn>>((resolve) => {
      const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...this.opts.timeoutsMs };

      const settle = (result: Result<Popcorn>) => {
        if (this.settleBoot === null) return;
        clearTimeout(timer);
        cleanup();
        if (!result.ok) {
          this.deinit();
        }
        resolve(result);
      };
      this.settleBoot = settle;

      const timer = setTimeout(() => {
        const error = err("timeout:init", { timeoutMs: timeoutsMs.boot });
        settle({ ok: false, error });
      }, timeoutsMs.boot);

      const onBootMessage = (event: MessageEvent<unknown>) => {
        const data = readWorkerEvent(event.data);
        check(data !== null);

        switch (data.type) {
          case "popcorn:boot-end":
            this.state = { status: "booted" };
            settle({ ok: true, data: this });
            break;
          case "popcorn:boot-fail": {
            const error = PopcornError.deserialize(data.payload);
            settle({ ok: false, error });
            break;
          }
          default:
            // user-level VM events are handled by the main worker listener.
            break;
        }
      };

      const cleanup = () => {
        this.settleBoot = null;
        this.vmWorker.removeEventListener("message", onBootMessage);
      };

      this.vmWorker.addEventListener("message", onBootMessage);
      toVm(this.vmWorker, { type: "popcorn:boot", payload: this.opts.beam });
    });
  }

  /**
   * Resolves after VM sent message to registered process.
   */
  public async send(
    target: string,
    payload?: AnyValue,
    opts?: PopcornSendOpts,
  ): Promise<Result<null>> {
    if (this.state.status !== "booted") {
      if (this.state.status === "closed") {
        return { ok: false, error: this.state.error };
      }
      return { ok: false, error: err("bridge:not-started", {}) };
    }

    const command = serializeSendPayload(
      target,
      payload ?? {},
      opts?.meta ?? {},
    );
    if (!command.ok) {
      return command;
    }

    const requestId = this.nextRequestId();
    const timeoutMs = { ...DEFAULT_TIMEOUTS_MS, ...this.opts.timeoutsMs }.send;

    return await new Promise<Result<null>>((resolve) => {
      const timer = setTimeout(() => {
        const wasMessageStale = this.pendingSends.delete(requestId);
        if (wasMessageStale) {
          resolve({ ok: false, error: err("timeout:send", { timeoutMs }) });
        }
      }, timeoutMs);

      this.pendingSends.set(requestId, (result) => {
        clearTimeout(timer);
        resolve(result);
      });
      toVm(this.vmWorker, {
        type: "popcorn:send",
        payload: { id: requestId, message: command.data },
      });
    });
  }

  public onEvent(handler: (event: PopcornEvent) => void): () => void {
    this.eventHandlers.add(handler);
    return () => {
      this.eventHandlers.delete(handler);
    };
  }

  public deinit(reason: VmExitReason = { reason: "deinit" }): void {
    if (this.state.status === "closed") {
      return;
    }

    const error = err("vm:exited", reason);
    if (this.settleBoot !== null) {
      this.settleBoot({ ok: false, error });
      return;
    }

    this.state = { status: "closed", error };
    for (const resolve of this.pendingSends.values()) {
      resolve({ ok: false, error });
    }
    this.pendingSends.clear();
    this.vmWorker.removeEventListener("message", this.onWorkerMessage);
    this.vmWorker.terminate();
    // we keep onEvent() callbacks across reboots
  }

  private emit(event: PopcornEvent): void {
    for (const handler of this.eventHandlers) {
      handler(event);
    }
  }

  private runJs(code: string): void {
    // Main thread; supports sync or async code, errors are thrown.
    indirectEval(code);
  }

  private completeSend(payload: SendCompletionPayload): void {
    const resolve = this.pendingSends.get(payload.id) ?? null;

    const didTimeout = resolve === null;
    if (didTimeout) return;

    this.pendingSends.delete(payload.id);
    const result = payload.result;
    resolve(
      result.ok
        ? { ok: true, data: null }
        : { ok: false, error: PopcornError.deserialize(result.error) },
    );
  }

  private nextRequestId(): string {
    this.requestSeq += 1;
    return `send:${this.requestSeq}`;
  }

  private handleOtpError(payload: OtpErrorPayload): void {
    const onError = this.opts.onError ?? defaultOnError;
    onError(payload);

    check(this.state.status === "booting" || this.state.status === "booted");

    // if failed while booting, settle early
    const booting = this.state.status === "booting";
    if (booting) {
      check(this.settleBoot !== null);

      const error = err("vm:exited", exitReason(payload));
      this.settleBoot({ ok: false, error });
      return;
    }

    this.deinit(exitReason(payload));
  }
}

function exitReason(payload: OtpErrorPayload): VmExitReason {
  switch (payload.kind) {
    case "abort":
      return { reason: "abort", data: payload.data };
    case "error":
      return { reason: "error", data: payload.data };
    case "exit":
      return { reason: "exit", data: payload.data };
    default:
      return unreachable();
  }
}

// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/eval#direct_and_indirect_eval
function indirectEval(code: string): unknown {
  return (0, eval)(code);
}

function canEval(): boolean {
  try {
    indirectEval("0");
    return true;
  } catch {
    return false;
  }
}

function defaultOnError(payload: OtpErrorPayload): void {
  switch (payload.kind) {
    case "abort":
      console.error(`${LOG_PREFIX} abort:`, payload.data);
      return;
    case "error":
      console.error(`${LOG_PREFIX} error:`, payload.data);
      return;
    case "exit":
      console.info(`${LOG_PREFIX} exit:`, payload.data);
      return;
    default:
      unreachable();
  }
}
