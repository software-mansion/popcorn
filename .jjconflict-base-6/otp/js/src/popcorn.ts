import { PopcornError, err, type Result } from "./errors";
import { RawTerm } from "./etf";
import {
  readWorkerEvent,
  serializeSendPayload,
  toVm,
  type PopcornEvent,
  type SendCompletionPayload,
} from "./events";
import type {
  AnyValue,
  BeamBootOptions,
  BeamTarget,
  OtpErrorPayload,
  Pid,
  RunJsRequest,
} from "./types";
import { base64ToBytes, check, objectWithKeys, unreachable } from "./utils";

type TrackedEntry = { value: unknown; cleanup?: () => void };

const TRACKED_REF_KEY = "popcorn_ref";
const PID_REF_KEY = "popcorn_pid";

export type PopcornOpts = {
  beam: Pick<BeamBootOptions, "manifestUrl" | "extraArgs">;
  timeoutsMs?: {
    boot?: number;
    send?: number;
  };
  onStdout?: (text: string) => void;
  onStderr?: (text: string) => void;
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
type SendFn = (
  target: string | Pid,
  payload?: AnyValue,
  opts?: PopcornSendOpts,
) => Promise<Result<null>>;
type RunJsFn = (args: AnyValue, send: SendFn) => AnyValue;

function createPidClass() {
  return class {
    public constructor(public readonly bytes: Uint8Array) {}
  };
}

function assertRunJsFn(value: unknown): asserts value is RunJsFn {
  check(typeof value === "function");
}

export class Popcorn {
  private vmWorker!: Worker;
  private state: PopcornState = { status: "created" };
  private readonly opts: PopcornOpts;
  private requestSeq = 0;
  private settleBoot: ((result: Result<Popcorn>) => void) | null = null;
  private readonly eventHandlers = new Set<(event: PopcornEvent) => void>();
  private readonly pendingSends = new Map<string, PendingSend>();
  private readonly trackedValues = new Map<number, TrackedEntry>();
  private trackedKeySeq = 0;

  private readonly TrackedValue = class {
    public constructor(
      public readonly value: unknown,
      public readonly cleanup?: () => void,
    ) {}
  };

  private Pid = createPidClass();
  private readonly onWorkerMessage = (event: MessageEvent<unknown>) => {
    const data = readWorkerEvent(event.data);
    check(data !== null);

    switch (data.type) {
      case "popcorn:boot-end":
      case "popcorn:boot-fail":
        return;
      case "otp:message":
        this.emit(this.reviveHandles(data.payload));
        return;
      case "otp:run_js":
        this.runJs(data.payload);
        return;
      case "otp:tracked-value-delete":
        this.deleteTrackedValue(data.payload);
        return;
      case "otp:stdout":
        this.handleStdout(data.payload);
        return;
      case "otp:stderr":
        this.handleStderr(data.payload);
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

    this.Pid = createPidClass();
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
    rawTarget: string | Pid,
    payload?: AnyValue,
    opts?: PopcornSendOpts,
  ): Promise<Result<null>> {
    if (this.state.status !== "booted") {
      if (this.state.status === "closed") {
        return { ok: false, error: this.state.error };
      }
      return { ok: false, error: err("bridge:not-started", {}) };
    }

    let target: BeamTarget;
    if (typeof rawTarget === "string" && rawTarget.length > 0) {
      target = { name: rawTarget };
    } else if (rawTarget instanceof this.Pid) {
      target = { pid: rawTarget.bytes };
    } else {
      return { ok: false, error: err("bridge:invalid-target", {}) };
    }

    const command = serializeSendPayload(
      target,
      this.serializeHandles(payload ?? {}),
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
      toVm(
        this.vmWorker,
        {
          type: "popcorn:send",
          payload: { id: requestId, message: command.data },
        },
        [command.data.etf.buffer],
      );
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
    this.clearTrackedValues();
    this.vmWorker.removeEventListener("message", this.onWorkerMessage);
    this.vmWorker.terminate();
    // we keep onEvent() callbacks across reboots
  }

  private clearTrackedValues(): void {
    for (const entry of this.trackedValues.values()) {
      try {
        entry.cleanup?.();
      } catch {}
    }
    this.trackedValues.clear();
  }

  private emit(event: PopcornEvent): void {
    for (const handler of this.eventHandlers) {
      handler(event);
    }
  }

  private async runJs(request: RunJsRequest): Promise<void> {
    let payload: AnyValue;
    try {
      const fn = this.jsWithCurrentEnv(request.code);
      assertRunJsFn(fn);
      const args = this.reviveHandles(request.args);
      const send: SendFn = (target, sendPayload, sendOpts) =>
        this.send(target, sendPayload, sendOpts);
      const result = await fn(args, send);
      payload = { ok: true, value: this.serializeHandles(result) ?? null };
    } catch (error) {
      check(error instanceof Error);
      payload = { ok: false, error: error.toString() };
    }

    const command = serializeSendPayload({ pid: request.replyTo }, payload, {});
    check(command.ok);
    toVm(
      this.vmWorker,
      {
        type: "popcorn:run-js-reply",
        payload: { message: command.data },
      },
      [command.data.etf.buffer],
    );
  }

  private jsWithCurrentEnv(code: string): unknown {
    const make = new Function(
      "TrackedValue",
      `"use strict"; return (${code});`,
    );
    return make(this.TrackedValue);
  }

  private reviveHandles(value: unknown): unknown {
    const key = trackedRefKey(value);
    if (key !== null) {
      const entry = this.trackedValues.get(key);
      check(entry !== undefined);
      return entry.value;
    }
    const pidToken = pidRefToken(value);
    if (pidToken !== null) {
      return new this.Pid(base64ToBytes(pidToken));
    }
    if (Array.isArray(value)) {
      return value.map((item) => this.reviveHandles(item));
    }
    const obj = objectWithKeys(value, []);
    if (obj !== null) {
      const revived: Record<string, unknown> = {};
      for (const [k, v] of Object.entries(obj)) {
        revived[k] = this.reviveHandles(v);
      }
      return revived;
    }
    return value;
  }

  private serializeHandles(value: unknown): unknown {
    if (value instanceof this.Pid) {
      // Splice the pid's own ETF bytes so binary_to_term revives a real pid.
      return RawTerm.fromExternal(value.bytes);
    }
    if (value instanceof this.TrackedValue) {
      this.trackedKeySeq += 1;
      const key = this.trackedKeySeq;
      this.trackedValues.set(key, {
        value: value.value,
        cleanup: value.cleanup,
      });
      return { [TRACKED_REF_KEY]: key };
    }
    if (Array.isArray(value)) {
      return value.map((item) => this.serializeHandles(item));
    }
    const obj = objectWithKeys(value, []);
    if (obj !== null) {
      const serialized: Record<string, unknown> = {};
      for (const [k, v] of Object.entries(obj)) {
        serialized[k] = this.serializeHandles(v);
      }
      return serialized;
    }
    return value;
  }

  private deleteTrackedValue(key: number): void {
    const entry = this.trackedValues.get(key);
    check(entry !== undefined);
    try {
      entry.cleanup?.();
    } finally {
      this.trackedValues.delete(key);
    }
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

  private handleStdout(text: string): void {
    const onStdout = this.opts.onStdout ?? defaultOnStdout;
    onStdout(text);
  }

  private handleStderr(text: string): void {
    const onStderr = this.opts.onStderr ?? defaultOnStderr;
    onStderr(text);
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

function trackedRefKey(value: unknown): number | null {
  const marker = objectWithKeys(value, [TRACKED_REF_KEY]);
  const hasOnlyMarker = marker !== null && Object.keys(marker).length === 1;
  if (!hasOnlyMarker) {
    return null;
  }
  const key = marker[TRACKED_REF_KEY];
  check(typeof key === "number");
  return key;
}

function pidRefToken(value: unknown): string | null {
  const marker = objectWithKeys(value, [PID_REF_KEY]);
  const hasOnlyMarker = marker !== null && Object.keys(marker).length === 1;
  if (!hasOnlyMarker) {
    return null;
  }
  const token = marker[PID_REF_KEY];
  check(typeof token === "string");
  return token;
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

function defaultOnStdout(text: string): void {
  console.log(`${LOG_PREFIX} stdout:`, text);
}

function defaultOnStderr(text: string): void {
  console.error(`${LOG_PREFIX} stderr:`, text);
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
