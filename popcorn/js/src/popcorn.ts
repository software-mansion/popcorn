import { PopcornError, err, type Result } from "./errors";
import { RawTerm, atom, tuple, type Mapper } from "./etf";
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
  BeamSendPayload,
  BeamTarget,
  OtpErrorPayload,
  Pid,
  RunJsRequest,
  TtySize,
} from "./types";
import { base64ToBytes, check, objectWithKeys, unreachable } from "./utils";

type TrackedEntry = { value: unknown; cleanup?: () => void };
type PendingTracked = TrackedEntry & { key: number };

const TRACKED_REF_KEY = "popcorn_ref";
const PID_REF_KEY = "popcorn_pid";
const UTF8 = new TextEncoder();
const STDIN_QUEUE_CAPACITY_BYTES = 64 * 1024;
const DEFAULT_TTY_SIZE: TtySize = { columns: 80, rows: 24 };

/** Output type for a terminal. */
type TtyOutput = "text" | "bytes";
type OutputChunk<Output extends TtyOutput> = Output extends "bytes"
  ? Uint8Array
  : string;

/** Browser VM configuration. */
export type PopcornOpts<Output extends TtyOutput = "text"> = {
  beam?: Pick<BeamBootOptions, "emulatorArgs" | "extraArgs" | "env"> & {
    /**
     * Asset directory URL.
     *
     * Must end with `/`. Defaults to `otp/` next to the worker.
     */
    otpAssetsRoot?: string;
  };
  tty?: {
    /** Initial terminal size. Defaults to 80 columns and 24 rows. */
    size?: TtySize;
    /** Output callback format. Defaults to `text` with streamed UTF-8 decoding. Use `bytes` for raw chunks. */
    output?: Output;
  };
  timeoutsMs?: {
    /**
     * Maximum wait for the VM bridge.
     *
     * Defaults to 10 000 ms.
     */
    boot?: number;
    /**
     * Maximum wait for entrypoint startup after bridge readiness.
     *
     * Defaults to 60 000 ms.
     */
    appStartup?: number;
    /**
     * Maximum wait for a send resolving its process target.
     *
     * Defaults to 5 000 ms.
     */
    send?: number;
  };

  /**
   * Receives stdout.
   *
   * Defaults to `console.log`.
   * When `tty.output` is "bytes", we pass an `ArrayBuffer` as an argument and `string` otherwise
   */
  onStdout?: (chunk: OutputChunk<Output>) => void;
  /**
   * Receives stderr.
   *
   * Defaults to `console.error`.
   * When `tty.output` is "bytes", we pass an `ArrayBuffer` as an argument and `string` otherwise
   */
  onStderr?: (chunk: OutputChunk<Output>) => void;
  /**
   * Receives VM errors and exits before shutdown.
   *
   * Defaults to console output.
   */
  onError?: (event: OtpErrorPayload) => void;
  /**
   * Module worker URL.
   *
   * Defaults to the worker included with the package.
   */
  workerUrl?: string | URL;
};

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
type OutputHandlers = {
  stdout: (chunk: Uint8Array) => void;
  stderr: (chunk: Uint8Array) => void;
};
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
  appStartup: 60_000,
  send: 5_000,
};

const LOG_PREFIX = "[Popcorn]";

const DEFAULT_PROXY_NAME = "popcorn_proxy";
const DEFAULT_CALL_TIMEOUT_MS = 5_000;

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
) => Promise<Result<null>>;
type RunJsActions = { send: SendFn } & GenServer;
type RunJsFn = (args: AnyValue, actions: RunJsActions) => AnyValue;

type CallOpts = { timeoutMs?: number; proxy?: string };
type PendingCall = {
  settle: (result: Result<AnyValue>) => void;
  target: string | Pid;
  timeoutMs: number;
};
type ProxyReply =
  | { ok: true; value: AnyValue }
  | {
      ok: false;
      error:
        | { kind: "noproc" }
        | { kind: "exit"; reason: string }
        | { kind: "unserializable" }
        | { kind: "timeout" };
    };
export type GenServer = {
  /**
   * Calls a GenServer by registered name or {@link Pid} and waits for its reply.
   *
   * Requires a running `Popcorn.Proxy`, registered as `popcorn_proxy` by default.
   *
   * @param opts - `timeoutMs` defaults to 5 000 ms. `proxy` allows to select another registered proxy.
   * @returns A {@link Result} with the reply or a bridge, VM, timeout, or GenServer error.
   *
   * GenServer failures use `genserver:noproc`, `genserver:exit`, or `genserver:unserializable`.
   * A call timeout does not cancel server work.
   */
  call(
    target: string | Pid,
    request?: AnyValue,
    opts?: CallOpts,
  ): Promise<Result<AnyValue>>;

  /**
   * Sends a cast through the proxy.
   *
   * Success confirms delivery to the proxy
   * @see {@link call}
   * @param opts - `proxy` allows to select another registered proxy.
   */
  cast(
    target: string | Pid,
    request?: AnyValue,
    opts?: { proxy?: string },
  ): Promise<Result<null>>;
};

function createPidClass() {
  return class {
    public constructor(public readonly bytes: Uint8Array) {}
  };
}

function assertRunJsFn(value: unknown): asserts value is RunJsFn {
  check(typeof value === "function");
}

/**
 * A BEAM VM in a browser worker.
 *
 * Use {@link Popcorn.init} to create and start an instance.
 **/
export class Popcorn<Output extends TtyOutput = "text"> {
  private vmWorker!: Worker;
  private state: PopcornState = { status: "created" };
  private readonly opts: PopcornOpts<Output>;
  private readonly ttySize: TtySize;
  private output: OutputHandlers;
  private requestSeq = 0;
  private settleBoot: ((result: Result<Popcorn<Output>>) => void) | null = null;
  private readonly eventHandlers = new Set<(event: PopcornEvent) => void>();
  private readonly pendingSends = new Map<string, PendingSend>();
  private readonly pendingCalls = new Map<string, PendingCall>();
  private callSeq = 0;
  private readonly trackedValues = new Map<number, TrackedEntry>();
  private trackedKeySeq = 0;
  private io = createIoState();
  private vmReady = false;

  public readonly genserver: GenServer = {
    call: (target, request, opts) => this.call(target, request, opts),
    cast: (target, request, opts) => this.cast(target, request, opts),
  };

  private readonly TrackedValue = class {
    public constructor(
      public readonly value: unknown,
      public readonly cleanup?: () => void,
    ) {}
  };

  private Pid = createPidClass();
  private readonly onWorkerMessage = (event: MessageEvent<unknown>) => {
    const data = readWorkerEvent(event.data);

    switch (data.type) {
      case "popcorn:boot-vm-ready":
      case "popcorn:boot-end":
      case "popcorn:boot-fail":
        return;
      case "otp:message":
        this.emit(this.reviveHandles(data.payload));
        return;
      case "otp:run_js":
        this.vmReady = true;
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
      case "otp:stdin-consumed":
        check(data.payload > 0 && data.payload <= this.io.stdin.reservedBytes);
        this.io.stdin.reservedBytes -= data.payload;
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

  /**
   * Creates the worker.
   *
   * Call {@link boot} to start the VM.
   **/
  public constructor(opts: PopcornOpts<Output> = {}) {
    const ttySize = opts.tty?.size ?? DEFAULT_TTY_SIZE;
    check(isValidTtySize(ttySize));
    check(
      opts.beam?.otpAssetsRoot === undefined ||
        opts.beam.otpAssetsRoot.endsWith("/"),
      "otpAssetsRoot must end with a slash",
    );
    this.opts = {
      ...opts,
      beam: {
        ...opts.beam,
        emulatorArgs:
          opts.beam?.emulatorArgs ??
          schedulers({ base: 1, dirtyCpu: 1, dirtyIo: 1 }),
      },
    };
    this.ttySize = { ...ttySize };
    this.output = resolveOutputHandlers(opts);
    this.spawnWorker();
  }

  private spawnWorker(): void {
    this.vmWorker = this.opts.workerUrl
      ? new Worker(this.opts.workerUrl, { type: "module" })
      : // Keep this as one expression so Vite recognizes and bundles the worker.
        new Worker(new URL("./worker.mjs", import.meta.url), {
          type: "module",
        });
    this.vmWorker.addEventListener("message", this.onWorkerMessage);
  }

  /**
   * Creates an instance and waits for {@link boot}.
   *
   * For startup messages, use the constructor and register {@link onEvent} before boot.
   *
   * @returns Ok tuple or `runtime:eval-unavailable` if the page blocks JavaScript evaluation.
   */
  public static async init<Output extends TtyOutput = "text">(
    opts: PopcornOpts<Output> = {},
  ): Promise<Result<Popcorn<Output>>> {
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

  /**
   * Starts the VM and waits for its bridge and entrypoint application.
   *
   * Without an entrypoint, waits only for the bridge.
   *
   * After shutdown, starts a fresh VM with the original options.
   *
   * @returns Ok tuple with `this` if boot completes or error tuple.
   *
   * @example
   * ```ts
   * const popcorn = new Popcorn({});
   * popcorn.onEvent((message) => console.log(message));
   * const result = await popcorn.boot();
   * if (!result.ok) throw result.error;
   * ```
   */
  public async boot(): Promise<Result<Popcorn<Output>>> {
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
    this.io = createIoState();
    this.output = resolveOutputHandlers(this.opts);
    this.state = { status: "booting" };

    return await new Promise<Result<Popcorn<Output>>>((resolve) => {
      const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...this.opts.timeoutsMs };

      const settle = (result: Result<Popcorn<Output>>) => {
        if (this.settleBoot === null) return;
        clearTimeout(timer);
        cleanup();
        if (!result.ok) {
          this.deinit();
        }
        resolve(result);
      };
      this.settleBoot = settle;

      const startPhase = (timeoutMs: number) =>
        setTimeout(() => {
          const error = err("timeout:init", { timeoutMs });
          settle({ ok: false, error });
        }, timeoutMs);

      // The VM phase covers module instantiation and bridge readiness; the
      // app phase covers the entrypoint's application tree, which runs
      // arbitrary user startup code and can be much slower.
      let timer = startPhase(timeoutsMs.boot);

      const onBootMessage = (event: MessageEvent<unknown>) => {
        const data = readWorkerEvent(event.data);

        switch (data.type) {
          case "popcorn:boot-vm-ready":
            clearTimeout(timer);
            timer = startPhase(timeoutsMs.appStartup);
            break;
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
      toVm(this.vmWorker, {
        type: "popcorn:boot",
        payload: { ...this.opts.beam, ttySize: this.ttySize },
      });
    });
  }

  /**
   * Queues terminal input.
   *
   * Encodes strings as UTF-8 and copies byte arrays. Does not append a newline.
   *
   * Returns `stdio:overflow` if the chunk exceeds the remaining 64 KiB queue capacity.
   * An overflow leaves the queue unchanged.
   */
  public writeStdin(chunk: string | Uint8Array): Result<null> {
    if (this.state.status === "closed") {
      return { ok: false, error: this.state.error };
    }
    check(this.state.status === "booted");

    const bytes = toBytes(chunk);
    check(bytes.byteLength > 0);

    const attemptedBytes = this.io.stdin.reservedBytes + bytes.byteLength;
    if (attemptedBytes > STDIN_QUEUE_CAPACITY_BYTES) {
      const error = err("stdio:overflow", {
        capacityBytes: STDIN_QUEUE_CAPACITY_BYTES,
        attemptedBytes,
      });
      return { ok: false, error };
    }

    this.io.stdin.reservedBytes = attemptedBytes;
    const event = { type: "popcorn:stdin", payload: { chunk: bytes } } as const;
    toVm(this.vmWorker, event, [bytes.buffer]);

    return { ok: true, data: null };
  }

  /**
   * Sends new terminal dimensions to a booted VM.
   *
   * Each dimension must be between 1 and 65,535.
   */
  public resizeTty(columns: number, rows: number): Result<null> {
    if (this.state.status === "closed") {
      return { ok: false, error: this.state.error };
    }
    check(this.state.status === "booted");
    check(isValidTtySize({ columns, rows }));

    toVm(this.vmWorker, {
      type: "popcorn:tty-resize",
      payload: { columns, rows },
    });
    return { ok: true, data: null };
  }

  /**
   * Sends a payload to a registered process name or a {@link Pid} from this VM boot.
   *
   * The process receives `{wasm, Payload}`.
   * A send timeout does not cancel delivery.
   * Uses the value conversions in {@link AnyValue}. An omitted, `null`, or `undefined` payload becomes an empty map.
   *
   * @returns Ok tuple or `bridge:not-started` before boot and `vm:exited` after shutdown.
   *
   * @example
   * Send an Erlang `{ok, <<"value">>}` tuple to a registered `receiver` process.
   *
   * ```ts
   * const result = await popcorn.send("receiver", tuple(atom("ok"), "value"));
   * if (!result.ok) throw result.error;
   * ```
   *
   * @see {@link AnyValue}
   * @see {@link atom}
   * @see {@link tuple}
   */
  public async send(
    rawTarget: string | Pid,
    payload?: AnyValue,
  ): Promise<Result<null>> {
    if (this.state.status !== "booted") {
      if (this.state.status === "closed") {
        return { ok: false, error: this.state.error };
      }
      return { ok: false, error: err("bridge:not-started", {}) };
    }

    return await this.sendBridge(rawTarget, payload);
  }

  private async sendBridge(
    rawTarget: string | Pid,
    payload?: AnyValue,
  ): Promise<Result<null>> {
    let target: BeamTarget;
    if (typeof rawTarget === "string" && rawTarget.length > 0) {
      target = { name: rawTarget };
    } else if (rawTarget instanceof this.Pid) {
      target = { pid: rawTarget.bytes };
    } else {
      return { ok: false, error: err("bridge:invalid-target", {}) };
    }

    const tracked: PendingTracked[] = [];
    const command = serializeSendPayload(
      target,
      payload ?? {},
      this.handleMapper(tracked),
    );
    if (!command.ok) {
      return command;
    }
    for (const { key, value, cleanup } of tracked) {
      this.trackedValues.set(key, { value, cleanup });
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

  /**
   * Registers a handler for BEAM message payloads.
   *
   * Messages with no handlers are lost. Startup messages can arrive before {@link boot} resolves.
   * VM errors and terminal output use the callbacks in {@link PopcornOpts}.
   *
   * @returns a function that removes the handler.
   */
  public onEvent(handler: (event: PopcornEvent) => void): () => void {
    this.eventHandlers.add(handler);
    return () => {
      this.eventHandlers.delete(handler);
    };
  }

  /**
   * Stops the worker and completes pending sends and calls with `vm:exited`.
   *
   * Releases tracked values and runs their cleanup callbacks.
   * Keeps event handlers for the next boot. Repeated calls have no effect.
   */
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
    this.vmReady = false;
    for (const resolve of this.pendingSends.values()) {
      resolve({ ok: false, error });
    }
    this.pendingSends.clear();
    for (const pending of this.pendingCalls.values()) {
      pending.settle({ ok: false, error });
    }
    this.pendingCalls.clear();
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
    const popcorn = objectWithKeys(event, ["_popcorn"])?._popcorn;
    const envelope = objectWithKeys(popcorn, ["t", "id", "payload"]);

    if (envelope !== null) {
      check(envelope.t === "proxy");
      this.completeCall(envelope.id as string, envelope.payload);
      return;
    }

    if (this.eventHandlers.size === 0) {
      console.warn(
        `${LOG_PREFIX} Dropped message with no event handlers`,
        event,
      );
    }
    for (const handler of this.eventHandlers) {
      handler(event);
    }
  }

  private completeCall(id: string, payload: unknown): void {
    const pending = this.pendingCalls.get(id);
    const lateReply = pending === undefined;
    if (lateReply) return;

    this.pendingCalls.delete(id);
    pending.settle(this.parseCallReply(pending, payload));
  }

  private parseCallReply(
    pending: PendingCall,
    payload: unknown,
  ): Result<AnyValue> {
    const reply = payload as ProxyReply;
    if (reply.ok) return { ok: true, data: reply.value };

    switch (reply.error.kind) {
      case "noproc": {
        const rawTarget = pending.target;
        const isName = typeof rawTarget === "string";
        const target = isName ? rawTarget : "<pid>";
        return {
          ok: false,
          error: err("genserver:noproc", { target }),
        };
      }
      case "exit":
        return {
          ok: false,
          error: err("genserver:exit", { reason: reply.error.reason }),
        };
      case "unserializable":
        return { ok: false, error: err("genserver:unserializable", {}) };
      case "timeout":
        return {
          ok: false,
          error: err("timeout:call", { timeoutMs: pending.timeoutMs }),
        };
      default:
        unreachable();
    }
  }

  private async call(
    rawTarget: string | Pid,
    request: AnyValue,
    opts?: CallOpts,
  ): Promise<Result<AnyValue>> {
    if (this.state.status !== "booted") {
      if (this.state.status === "closed") {
        return { ok: false, error: this.state.error };
      }
      return { ok: false, error: err("bridge:not-started", {}) };
    }

    return await this.callBridge(rawTarget, request, opts);
  }

  private async callBridge(
    rawTarget: string | Pid,
    request: AnyValue,
    opts?: CallOpts,
  ): Promise<Result<AnyValue>> {
    const timeoutMs = opts?.timeoutMs ?? DEFAULT_CALL_TIMEOUT_MS;
    const proxy = opts?.proxy ?? DEFAULT_PROXY_NAME;
    const id = this.nextCallId();

    const result = new Promise<Result<AnyValue>>((resolve) => {
      const timer = setTimeout(() => {
        const isUnresolved = this.pendingCalls.delete(id);
        if (isUnresolved) {
          resolve({ ok: false, error: err("timeout:call", { timeoutMs }) });
        }
      }, timeoutMs);

      this.pendingCalls.set(id, {
        target: rawTarget,
        timeoutMs,
        settle: (settled) => {
          clearTimeout(timer);
          resolve(settled);
        },
      });
    });

    const sent = await this.sendBridge(proxy, {
      kind: "call",
      id,
      target: rawTarget,
      request: request,
      timeout_ms: timeoutMs,
    });

    if (!sent.ok) {
      const pending = this.pendingCalls.get(id);
      this.pendingCalls.delete(id);
      pending?.settle({ ok: false, error: sent.error });
    }

    return result;
  }

  private async cast(
    rawTarget: string | Pid,
    request: AnyValue,
    opts?: { proxy?: string },
  ): Promise<Result<null>> {
    if (this.state.status !== "booted") {
      if (this.state.status === "closed") {
        return { ok: false, error: this.state.error };
      }
      return { ok: false, error: err("bridge:not-started", {}) };
    }

    return await this.castBridge(rawTarget, request, opts);
  }

  private async castBridge(
    rawTarget: string | Pid,
    request: AnyValue,
    opts?: { proxy?: string },
  ): Promise<Result<null>> {
    const proxy = opts?.proxy ?? DEFAULT_PROXY_NAME;
    return await this.sendBridge(proxy, {
      kind: "cast",
      target: rawTarget,
      request: request,
    });
  }

  private nextCallId(): string {
    this.callSeq += 1;
    return `call:${this.callSeq}`;
  }

  private async runJs(request: RunJsRequest): Promise<void> {
    let payload: AnyValue;
    try {
      const fn = this.jsWithCurrentEnv(request.code);
      assertRunJsFn(fn);
      const args = this.reviveHandles(request.args);
      check(this.vmReady);
      const actions: RunJsActions = {
        send: (target, payload) => this.sendBridge(target, payload),
        call: (target, payload, opts) => this.callBridge(target, payload, opts),
        cast: (target, payload, opts) => this.castBridge(target, payload, opts),
      };
      const result = await fn(args, actions);
      const value = request.return === "ref" ? this.asRef(result) : result;
      payload = { ok: true, value: value ?? null };
    } catch (error) {
      check(error instanceof Error);
      payload = { ok: false, error: error.toString() };
    }

    const target = { pid: request.replyTo };
    const tracked: PendingTracked[] = [];
    const command = serializeSendPayload(
      target,
      payload,
      this.handleMapper(tracked),
    );
    if (command.ok) {
      for (const { key, value, cleanup } of tracked) {
        this.trackedValues.set(key, { value, cleanup });
      }
      this.sendRunJsReply(command.data);
      return;
    }

    const failure = serializeSendPayload(target, {
      ok: false,
      error: { unserializable: command.error.data.reason },
    });
    check(failure.ok);
    this.sendRunJsReply(failure.data);
  }

  private asRef(value: unknown): unknown {
    if (value instanceof this.TrackedValue) return value;
    return new this.TrackedValue(value);
  }

  private sendRunJsReply(message: BeamSendPayload): void {
    toVm(
      this.vmWorker,
      { type: "popcorn:run-js-reply", payload: { message } },
      [message.etf.buffer],
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

  /** Maps pids and `TrackedValue`s during encoding, collecting handles into
   * `tracked` for the caller to register once encoding succeeds. */
  private handleMapper(tracked: PendingTracked[]): Mapper {
    return (value) => {
      if (value instanceof this.Pid) {
        return RawTerm.fromExternal(value.bytes);
      }
      if (value instanceof this.TrackedValue) {
        const key = (this.trackedKeySeq += 1);
        tracked.push({ key, value: value.value, cleanup: value.cleanup });
        return { [TRACKED_REF_KEY]: key };
      }
      return value;
    };
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

  private handleStdout(chunk: Uint8Array): void {
    this.output.stdout(chunk);
  }

  private handleStderr(chunk: Uint8Array): void {
    this.output.stderr(chunk);
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

/**
 * Thread counts for {@link schedulers}.
 *
 * Each count must be positive.
 */
export type SchedulerOptions = {
  /** Regular schedulers. */
  base: number;
  /** Dirty CPU schedulers. */
  dirtyCpu: number;
  /** Dirty I/O schedulers. */
  dirtyIo: number;
};

/**
 * Builds `beam.emulatorArgs` for scheduler counts.
 *
 * Defaults to one scheduler of each type.
 */
export function schedulers(opts: SchedulerOptions): string[] {
  const { base, dirtyCpu, dirtyIo } = opts;
  check(base > 0);
  check(dirtyCpu > 0);
  check(dirtyIo > 0);

  return ["-S", base, "-SDcpu", dirtyCpu, "-SDio", dirtyIo].map(String);
}

function isValidTtySize({ columns, rows }: TtySize): boolean {
  const colInRange = 0 < columns && columns <= 0xffff;
  const rowInRange = 0 < rows && rows <= 0xffff;
  return colInRange && rowInRange;
}

function resolveOutputHandlers<Output extends TtyOutput>(
  opts: PopcornOpts<Output>,
): OutputHandlers {
  type BytesHandler = (chunk: Uint8Array) => void;
  type TextHandler = (chunk: string) => void;

  if (opts.tty?.output === "bytes") {
    const onStdout = opts.onStdout as BytesHandler | undefined;
    const onStderr = opts.onStderr as BytesHandler | undefined;
    return {
      stdout: onStdout ?? defaultOnStdoutBytes,
      stderr: onStderr ?? defaultOnStderrBytes,
    };
  }

  const stdoutDecoder = new TextDecoder();
  const stderrDecoder = new TextDecoder();
  const onStdout =
    (opts.onStdout as TextHandler | undefined) ?? defaultOnStdout;
  const onStderr =
    (opts.onStderr as TextHandler | undefined) ?? defaultOnStderr;
  return {
    stdout: (chunk) => decodeOutput(stdoutDecoder, onStdout, chunk),
    stderr: (chunk) => decodeOutput(stderrDecoder, onStderr, chunk),
  };
}

function decodeOutput(
  decoder: TextDecoder,
  onOutput: (chunk: string) => void,
  chunk: Uint8Array,
): void {
  const output = decoder.decode(chunk, { stream: true });
  if (output.length > 0) onOutput(output);
}

function createIoState() {
  return {
    stdin: {
      reservedBytes: 0,
    },
  };
}

function toBytes(chunk: string | Uint8Array): Uint8Array {
  return typeof chunk === "string" ? UTF8.encode(chunk) : chunk.slice();
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

function defaultOnStdout(chunk: string): void {
  console.log(`${LOG_PREFIX} stdout:`, chunk);
}

function defaultOnStderr(chunk: string): void {
  console.error(`${LOG_PREFIX} stderr:`, chunk);
}

function defaultOnStdoutBytes(chunk: Uint8Array): void {
  console.log(`${LOG_PREFIX} stdout:`, chunk);
}

function defaultOnStderrBytes(chunk: Uint8Array): void {
  console.error(`${LOG_PREFIX} stderr:`, chunk);
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
