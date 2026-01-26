import { IframeBridge, type IframeBridgeArgs } from "./bridge";

const INIT_VM_TIMEOUT_MS = 30_000;
const CALL_TIMEOUT_MS = 60_000;
const HEARTBEAT_TIMEOUT_MS = 60_000;

// postMessage data:
//  | { type: "stdout", value: string }
//  | { type: "stderr", value: string }
//  | { type: "init", value: string }
//  | { type: "call", value: ElixirRequest | ElixirResponse}
//  | { type: "callAck", value: ElixirAck }
//  | { type: "cast", value: ElixirRequest }
//  | { type: "heartbeat", value: void }
//  | { type: "reload", value: void }
//
// ElixirRequest: { requestId: number, process: string, action: string, args: any }
// ElixirResponse: { requestId: number, error: string } | { requestId: number, data: any }
// ElixirAck: { requestId: number }
const MESSAGES = {
  INIT: "popcorn-init",
  START_VM: "popcorn-startVm",
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
  RELOAD: "popcorn-reload",
} as const;

type InitialProcessName = string;
type ReloadReason = string;
type Message =
  | { type: "popcorn-init"; value: never }
  | { type: "popcorn-startVm"; value: InitialProcessName }
  | {
      type: "popcorn-call";
      value: {
        requestId: number;
        error?: AnySerializable;
        data?: AnySerializable;
      };
    }
  | { type: "popcorn-cast"; value: never } // only sent, not received
  | { type: "popcorn-callAck"; value: { requestId: number } }
  | { type: "popcorn-stdout"; value: string }
  | { type: "popcorn-stderr"; value: string }
  | { type: "popcorn-heartbeat"; value: never }
  | { type: "popcorn-reload"; value: ReloadReason };

const MESSAGES_TYPES = new Set(Object.values(MESSAGES));

/** Options for Popcorn.init() */
export type PopcornInitOptions = {
  /** DOM element to mount an iframe */
  container?: HTMLElement;
  /** Path to compiled Elixir bundle (`.avm` file). */
  bundlePath?: string;
  /** Handler for stderr messages. */
  onStderr?: (message: string) => void;
  /** Handler for stdout messages. */
  onStdout?: (message: string) => void;
  /** Heartbeat timeout in milliseconds. If an iframe doesn't respond within this time, it is reloaded. */
  heartbeatTimeoutMs?: number;
  /** Directory containing Wasm and scripts used inside iframe. */
  wasmDir?: string;
  /** Enable debug logging. */
  debug?: boolean;
};

/** Options for cast method */
type CastOptions = {
  /** Receiver process name. */
  process?: string;
};

/** Options for call method */
type CallOptions = {
  /** Registered Elixir process name. */
  process?: string;
  /** Timeout (in milliseconds) for the call */
  timeoutMs?: number;
};

type CallResult = {
  /** Serialized value returned from Elixir */
  data: AnySerializable;
  /** Amount of time it took to process the call */
  durationMs: number;
  /** Optional error if call failed */
  error?: AnySerializable;
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type AnySerializable = any;

type LogType = "stdout" | "stderr";
type LogListener = (message: string) => void;
type LogListeners = Record<LogType, Set<LogListener>>;

type CallData = {
  acknowledged: boolean;
  startTimeMs: number;
  resolve: (result: CallResult) => void;
  reject: (error: { error: unknown; durationMs: number }) => void;
};

type AwaitedMessage = {
  type: string;
  resolve?: (value: AnySerializable) => void;
};

class PopcornDeinitializedError extends Error {}

type State = { status: "uninitialized" } | { status: "ready" };

const INIT_TOKEN = Symbol();
const IFRAME_URL = new URL("./iframe.mjs", import.meta.url).href;

/**
 * Manages Elixir by setting up iframe, WASM module, and event listeners. Used to sent messages to Elixir processes.
 */
export class Popcorn {
  public heartbeatTimeoutMs: number | null = null;

  private bridge: IframeBridge<Message> | null = null;
  private bridgeConfig: IframeBridgeArgs<Message>;
  private debug = false;
  private bundleURL: string;
  private initProcess: string | null = null;

  private state: State = { status: "uninitialized" };

  private requestId = 0;
  private calls = new Map<number, CallData>();
  private logListeners: LogListeners = {
    stdout: new Set(),
    stderr: new Set(),
  };

  private awaitedMessage: AwaitedMessage | null = null;
  private heartbeatTimeout: ReturnType<typeof setTimeout> | null = null;

  private constructor(
    params: PopcornInitOptions & { container: HTMLElement },
    token: symbol,
  ) {
    if (token !== INIT_TOKEN) throwError({ t: "private_constructor" });

    const bundlePath = params.bundlePath ?? "/bundle.avm";
    const bundleURL = new URL(bundlePath, import.meta.url);

    this.debug = params.debug ?? false;
    this.bundleURL = bundleURL.href;

    this.bridgeConfig = {
      container: params.container,
      script: { url: IFRAME_URL, entrypoint: "runIFrame" },
      config: { "bundle-path": this.bundleURL },
      debug: true,
      messageFilter: isPopcornPayload,
      onMessage: this.iframeHandler.bind(this),
    };

    this.logListeners.stdout.add(params.onStdout ?? console.log);
    this.logListeners.stdout.add(params.onStderr ?? console.warn);
    this.heartbeatTimeoutMs = params.heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
  }

  /**
   * Creates an iframe and sets up communication channels.
   * Returns after Elixir code calls `Popcorn.Wasm.register/1`.
   *
   * @example
   * import { Popcorn } from "@swmansion/popcorn";
   * const popcorn = await Popcorn.init({
   *   onStdout: console.log,
   *   onStderr: console.error,
   *   debug: true,
   * });
   */
  static async init(options: PopcornInitOptions): Promise<Popcorn> {
    const { container, ...constructorParams } = options;
    const containerWithDefault = container ?? document.documentElement;

    const popcorn = new Popcorn(
      { ...constructorParams, container: containerWithDefault },
      INIT_TOKEN,
    );
    popcorn.trace("Main: init, params: ", { container, ...constructorParams });
    await popcorn.mount();
    return popcorn;
  }

  private async mount(): Promise<void> {
    if (this.bridge !== null) throwError({ t: "already_mounted" });
    this.assertStatus("uninitialized");
    this.trace("Main: mount, container: ", this.bridgeConfig.container);

    this.bridge = new IframeBridge<Message>(this.bridgeConfig);

    await this.awaitMessage(MESSAGES.INIT);
    this.trace("Main: iframe loaded");
    this.initProcess = await withTimeout(
      this.awaitMessage(MESSAGES.START_VM),
      INIT_VM_TIMEOUT_MS,
    );
    this.trace("Main: mounted, main process: ", this.initProcess);
    this.onHeartbeat();
  }

  /**
   * Sends a message to an Elixir process and awaits for the response.
   *
   * If Elixir doesn't respond in configured timeout, the returned promise will be rejected with "process timeout" error.
   *
   * Unless passed via options, the name passed in `Popcorn.Wasm.register/1` on the Elixir side is used.
   * Throws "Unspecified target process" if default process is not set and no process is specified.
   *
   * @example
   * const result = await popcorn.call(
   *   { action: "get_user", id: 123 },
   *   { process: "user_server", timeoutMs: 5_000 },
   * );
   * console.log(result.data); // Deserialized Elixir response
   * console.log(result.durationMs); // Entire call duration
   */
  async call(
    args: AnySerializable,
    { process, timeoutMs }: CallOptions,
  ): Promise<CallResult> {
    const targetProcess = process ?? this.initProcess;
    if (this.bridge === null) throwError({ t: "unmounted" });
    if (targetProcess === null) throwError({ t: "bad_target" });

    const requestId = this.requestId++;
    const callPromise = new Promise<CallResult>((resolve, reject) => {
      if (this.bridge === null) throwError({ t: "unmounted" });
      this.trace("Main: call: ", { requestId, process, args });
      this.bridge.send({
        type: MESSAGES.CALL,
        value: { requestId, process: targetProcess, args },
      });

      this.calls.set(requestId, {
        acknowledged: false,
        startTimeMs: performance.now(),
        resolve,
        reject,
      });
    });

    const result = await withTimeout(callPromise, timeoutMs ?? CALL_TIMEOUT_MS);
    this.calls.delete(requestId);
    return result;
  }

  /**
   * Sends a message to an Elixir process (default or from options) and returns immediately.
   *
   * Unless passed via options, the name passed in `Popcorn.Wasm.register/1` on the Elixir side is used.
   * Throws "Unspecified target process" if default process is not set and no process is specified.
   */
  cast(args: AnySerializable, { process }: CastOptions): void {
    this.assertStatus("ready");

    const targetProcess = process ?? this.initProcess;
    if (this.bridge === null) throwError({ t: "unmounted" });
    if (targetProcess === null) throwError({ t: "bad_target" });

    const requestId = this.requestId++;
    this.trace("Main: cast: ", { requestId, process, args });
    this.bridge.send({
      type: MESSAGES.CAST,
      value: { requestId, process: targetProcess, args },
    });
  }

  /**
   * Destroys an iframe and resets the instance.
   */
  deinit() {
    if (this.bridge === null) throwError({ t: "unmounted" });
    this.trace("Main: deinit");
    this.bridge.deinit();
    this.awaitedMessage = null;
    this.logListeners.stdout.clear();
    this.logListeners.stderr.clear();
    for (const callData of this.calls.values()) {
      const durationMs = performance.now() - callData.startTimeMs;
      callData.reject({
        error: new PopcornDeinitializedError(
          "Call cancelled due to instance deinit",
        ),
        durationMs,
      });
    }
  }

  /**
   * Registers a log listener that will be called when output of the specified type is received.
   */
  registerLogListener(listener: LogListener, type: LogType): void {
    this.logListeners[type].add(listener);
  }

  /**
   * Unregisters a previously registered log listener.
   */
  unregisterLogListener(listener: LogListener, type: LogType): void {
    this.logListeners[type].delete(listener);
  }

  private notifyLogListeners(type: LogType, message: string): void {
    this.logListeners[type].forEach((listener) => {
      listener(message);
    });
  }

  private iframeHandler(data: Message) {
    const awaitedMessage = this.awaitedMessage;
    if (awaitedMessage && data.type == awaitedMessage.type) {
      this.awaitedMessage = null;
      awaitedMessage.resolve?.(data.value);
      return;
    }

    if (data.type === MESSAGES.STDOUT) {
      this.notifyLogListeners("stdout", data.value);
    } else if (data.type === MESSAGES.STDERR) {
      this.notifyLogListeners("stderr", data.value);
    } else if (data.type === MESSAGES.CALL) {
      this.onCall(data.value);
    } else if (data.type === MESSAGES.CALL_ACK) {
      this.onCallAck(data.value);
    } else if (data.type === MESSAGES.HEARTBEAT) {
      this.onHeartbeat();
    } else if (data.type === MESSAGES.RELOAD) {
      this.reloadIframe();
    } else {
      throwError({ t: "assert" });
    }
  }

  private onCallAck({ requestId }: { requestId: number }): void {
    this.assertStatus("ready");
    this.trace("Main: onCallAck: ", { requestId });
    const callData = this.calls.get(requestId);
    if (callData === undefined) throwError({ t: "bad_ack" });

    this.calls.set(requestId, { ...callData, acknowledged: true });
  }

  private onCall({
    requestId,
    error,
    data,
  }: {
    requestId: number;
    error?: AnySerializable;
    data?: AnySerializable;
  }): void {
    this.assertStatus("ready");
    this.trace("Main: onCall: ", { requestId, error, data });
    const callData = this.calls.get(requestId);
    if (callData === undefined) throwError({ t: "bad_call" });
    if (!callData.acknowledged) throwError({ t: "no_acked_call" });

    this.calls.delete(requestId);

    const durationMs = performance.now() - callData.startTimeMs;
    if (error !== undefined) {
      callData.reject({ error, durationMs });
    } else {
      callData.resolve({ data, durationMs });
    }
  }

  private onHeartbeat(): void {
    if (this.heartbeatTimeout) {
      clearTimeout(this.heartbeatTimeout);
    }
    this.heartbeatTimeout = setTimeout(() => {
      this.trace("Main: heartbeat lost");
      this.reloadIframe("heartbeat_lost");
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    }, this.heartbeatTimeoutMs!);
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  private reloadIframe(_reason = "other"): void {
    if (this.bridge === null) {
      throw new Error("WASM iframe not mounted for reload");
    }

    if (document.hidden) {
      this.trace("Main: reloading iframe skipped, window not visible");
      return;
    }
    this.trace("Main: reloading iframe");
    this.deinit();
    this.mount();
  }

  private awaitMessage(type: string): Promise<AnySerializable> {
    if (this.awaitedMessage) {
      throwError({
        t: "already_awaited",
        messageType: this.awaitedMessage.type,
        awaitedMessageType: type,
      });
    }

    this.awaitedMessage = { type };

    return new Promise((resolve) => {
      if (!this.awaitedMessage) throwError({ t: "assert" });
      this.awaitedMessage.resolve = resolve;
    });
  }

  trace(...messages: unknown[]): void {
    if (this.debug) {
      console.debug(...messages);
    }
  }

  private assertStatus(status: State["status"]): void {
    const currentStatus = this.state.status;
    if (currentStatus !== status) {
      throwError({
        t: "bad_status",
        status: currentStatus,
        expectedStatus: status,
      });
    }
  }
}

async function withTimeout<T>(promise: Promise<T>, ms: number): Promise<T> {
  let timeout: ReturnType<typeof setTimeout> | null = null;
  const timeoutPromise = new Promise<never>((_resolve, reject) => {
    timeout = setTimeout(() => {
      reject("Promise timeout");
    }, ms);
  });

  const result = await Promise.race([promise, timeoutPromise]);

  if (!timeout) throwError({ t: "assert" });
  clearTimeout(timeout);
  return result;
}

type ErrorData =
  | { t: "assert" }
  | {
      t: "bad_status";
      status: State["status"];
      expectedStatus: State["status"];
    }
  | { t: "private_constructor" }
  | { t: "bad_call" }
  | { t: "no_acked_call" }
  | { t: "bad_ack" }
  | { t: "unmounted" }
  | { t: "bad_target" }
  | {
      t: "already_awaited";
      messageType: string;
      awaitedMessageType: string;
    }
  | { t: "already_mounted" };

function throwError(error: ErrorData): never {
  switch (error.t) {
    case "assert":
      throw new Error("Assertion error");
    case "bad_status":
      throw new Error(
        `Unexpected status transition. Instance in ${error.status} status, expected ${error.expectedStatus}`,
      );
    case "private_constructor":
      throw new Error(
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    case "bad_call":
      throw new Error("Response for non-existent call");
    case "no_acked_call":
      throw new Error("Response for non-acknowledged call");
    case "bad_ack":
      throw new Error("Ack for non-existent call");
    case "unmounted":
      throw new Error("WASM iframe not mounted");
    case "bad_target":
      throw new Error("Unspecified target process");

    case "already_awaited":
      throw new Error(
        `Cannot await message ${error.messageType} when a message ${error.awaitedMessageType} is already awaited on`,
      );
    case "already_mounted":
      throw new Error("Iframe already mounted");
  }
}

function isPayloadShape(
  payload: object,
): payload is { type: string; value: AnySerializable } {
  const hasType = Object.hasOwn(payload, "type");
  const hasValue = Object.hasOwn(payload, "value");
  const hasFields = hasType && hasValue;
  if (!hasFields) return false;
  return true;
}

function isPopcornPayload(payload: unknown): payload is Message {
  const isObject = typeof payload === "object" && payload !== null;
  if (!isObject) return false;
  if (!isPayloadShape(payload)) return false;
  const hasOkFieldTypes = typeof payload.type === "string";
  if (!hasOkFieldTypes) return false;

  // TODO: may also check `value` under `debug`
  return (MESSAGES_TYPES as Set<string>).has(payload.type);
}
