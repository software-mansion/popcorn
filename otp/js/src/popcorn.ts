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
  };
  onError?: (event: OtpErrorPayload) => void;
};

export type PopcornSendOpts = {
  meta?: AnyValue;
};

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
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
  private vmWorker: Worker;
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
    const vmWorkerUrl = new URL("./worker.mjs", import.meta.url);
    this.vmWorker = new Worker(vmWorkerUrl, { type: "module" });
    this.opts = opts;
    this.vmWorker.addEventListener("message", this.onWorkerMessage);
  }

  public static async init(opts: PopcornOpts): Promise<Result<Popcorn>> {
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

    if (this.state.status === "closed") {
      return { ok: false, error: this.state.error };
    }

    if (this.state.status === "booting") {
      return {
        ok: false,
        error: err("internal:check", { detail: "Boot already in progress" }),
      };
    }

    this.state = { status: "booting" };

    return await new Promise<Result<Popcorn>>((resolve) => {
      const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...this.opts.timeoutsMs };

      let isSettled = false;

      const settle = (result: Result<Popcorn>) => {
        if (isSettled) return;
        isSettled = true;
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
            // User-level VM events are handled by the main worker listener.
            break;
        }
      };

      const cleanup = () => {
        this.settleBoot = null;
        this.vmWorker.removeEventListener("message", onBootMessage);
        if (this.state.status === "booting") {
          this.state = { status: "created" };
        }
      };

      this.vmWorker.addEventListener("message", onBootMessage);
      toVm(this.vmWorker, { type: "popcorn:boot", payload: this.opts.beam });
    });
  }

  /**
   * Resolves after the VM has attempted to put the event into the target
   * listener process mailbox. It does not wait for application-level handling by
   * the receiving Elixir process.
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
    return await new Promise<Result<null>>((resolve) => {
      this.pendingSends.set(requestId, resolve);
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
    this.eventHandlers.clear();
    this.vmWorker.terminate();
  }

  private emit(event: PopcornEvent): void {
    for (const handler of this.eventHandlers) {
      handler(event);
    }
  }

  private completeSend(payload: SendCompletionPayload): void {
    const resolve = this.pendingSends.get(payload.id) ?? null;
    check(resolve !== null);

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

    check(["booting", "booted"].includes(this.state.status));
    if (this.state.status === "booting") {
      return;
    }

    switch (payload.kind) {
      case "abort":
        this.deinit({ reason: "abort", data: payload.data });
        break;
      case "error":
        this.deinit({ reason: "error", data: payload.data });
        break;
      case "exit":
        this.deinit({ reason: "exit", data: payload.data });
        break;
      default:
        unreachable();
    }
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
