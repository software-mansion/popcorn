import { PopcornError, err, type Result } from "./errors";
import {
  readWorkerEvent,
  serializeSendPayload,
  toVm,
  type PopcornEvent,
  type SendCompletionPayload,
} from "./events";
import type { AnyValue, BeamBootOptions } from "./types";
import { check } from "./utils";

export type PopcornOpts = {
  beam: Pick<BeamBootOptions, "assetsUrl" | "searchPaths" | "extraArgs">;
  timeoutsMs?: {
    boot?: number;
  };
};

export type PopcornSendOpts = {
  meta?: AnyValue;
};

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
};

const LOG_PREFIX = "[Popcorn]";

type PopcornState = "created" | "booting" | "booted";
type PendingSend = (result: Result<null>) => void;

export class Popcorn {
  private vmWorker: Worker;
  private state: PopcornState = "created";
  private readonly opts: PopcornOpts;
  private requestSeq = 0;
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
      case "otp:abort":
        console.error(`${LOG_PREFIX} abort:`, data.payload);
        return;
      case "otp:error":
        console.error(`${LOG_PREFIX} error:`, data.payload);
        return;
      case "otp:exit":
        console.info(`${LOG_PREFIX} exit:`, data.payload);
        return;
      case "popcorn:send-end": {
        this.completeSend(data.payload);
        return;
      }
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
    if (this.state === "booted") {
      return { ok: true, data: this };
    }

    if (this.state === "booting") {
      return {
        ok: false,
        error: err("internal:check", { detail: "Boot already in progress" }),
      };
    }

    this.state = "booting";

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

      const timer = setTimeout(() => {
        const error = err("timeout:init", { timeoutMs: timeoutsMs.boot });
        settle({ ok: false, error });
      }, timeoutsMs.boot);

      const onBootMessage = (event: MessageEvent<unknown>) => {
        const data = readWorkerEvent(event.data);
        check(data !== null);

        switch (data.type) {
          case "popcorn:boot-end":
            this.state = "booted";
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
        this.vmWorker.removeEventListener("message", onBootMessage);
        if (this.state === "booting") {
          this.state = "created";
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
    if (this.state !== "booted") {
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

  public deinit(): void {
    this.state = "created";
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
    if (resolve === null) {
      return;
    }

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
}
