import { IframeBridge } from "./bridge";
import {
  INIT_TIMEOUT_MS,
  HEARTBEAT_TIMEOUT_MS,
  CALL_TIMEOUT_MS,
  MAX_RELOAD_N,
  MESSAGES,
  EVENT_NAMES,
} from "./types";
import { PopcornError, PopcornInternalError, throwError } from "./errors";

import type { IframeBridgeArgs } from "./bridge";
import type { IframeResponse, AnySerializable, ElixirEvent } from "./types";
import type { PopcornErrorCode, PopcornInternalErrorCode } from "./errors";

export { PopcornError, PopcornInternalError };
export type { PopcornErrorCode, PopcornInternalErrorCode };

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
  /** Handler called when Popcorn reloads due to iframe crash */
  onReload?: (reason: string) => void;
  /** Heartbeat timeout in milliseconds. If an iframe doesn't respond within this time, it is reloaded. */
  heartbeatTimeoutMs?: number;
  /** Directory containing Wasm and scripts used inside iframe. */
  wasmDir?: string;
  /** Enable debug logging. */
  debug?: boolean;
};

/** Options for cast method */
export type CastOptions = {
  /** Receiver process name. */
  process?: string;
};

/** Options for call method */
export type CallOptions = {
  /** Registered Elixir process name. */
  process?: string;
  /** Timeout (in milliseconds) for the call */
  timeoutMs?: number;
};

type CallResult =
  | {
      ok: true;
      /** Serialized value returned from Elixir */
      data: AnySerializable;
      /** Amount of time it took to process the call */
      durationMs: number;
    }
  | {
      ok: false;
      /** Error from failed call */
      error: Error;
      /** Amount of time it took to process the call */
      durationMs: number;
    };

type LogType = "stdout" | "stderr";
type LogListener = (message: string) => void;
type LogListeners = Record<LogType, Set<LogListener>>;

type CallData = {
  acknowledged: boolean;
  startTimeMs: number;
  resolve: (result: CallResult) => void;
};

type EventHandler = (eventName: string, payload: AnySerializable) => void;

type State =
  | { status: "uninitialized" }
  | { status: "mount" }
  | { status: "ready" }
  | { status: "reload" }
  | { status: "deinit" };

const INIT_TOKEN = Symbol();
const IFRAME_URL = new URL("./iframe.mjs", import.meta.url).href;

/**
 * Manages Elixir by setting up iframe, WASM module, and event listeners. Used to sent messages to Elixir processes.
 */
export class Popcorn {
  public heartbeatTimeoutMs: number | null = null;

  private onReloadCallback: (reason: string) => void;

  private bridge: IframeBridge | null = null;
  private bridgeConfig: IframeBridgeArgs;
  private debug = false;
  private bundleURL: string;
  private state: State = { status: "uninitialized" };
  private defaultReceiver: string | null = null;

  private requestId = 0;
  private calls = new Map<number, CallData>();
  private logListeners: LogListeners = {
    stdout: new Set(),
    stderr: new Set(),
  };

  private mountPromise: (() => void) | null = null;
  private heartbeatTimeout: ReturnType<typeof setTimeout> | null = null;
  private reloadN = 0;

  private eventHandlers: EventHandler[] = [];
  private preMountEvents: ElixirEvent[] = [];

  private constructor(
    params: PopcornInitOptions & { container: HTMLElement },
    token: symbol,
  ) {
    if (token !== INIT_TOKEN) throwError({ t: "private_constructor" });

    const bundlePath = params.bundlePath ?? "/bundle.avm";
    const bundleURL = new URL(bundlePath, import.meta.url);

    this.onReloadCallback = params.onReload ?? noop;
    this.debug = params.debug ?? false;
    this.bundleURL = bundleURL.href;

    this.bridgeConfig = {
      container: params.container,
      script: { url: IFRAME_URL, entrypoint: "initVm" },
      config: { "bundle-path": this.bundleURL },
      debug: true,
      onMessage: this.iframeHandler.bind(this),
    };

    this.logListeners.stdout.add(params.onStdout ?? console.log);
    this.logListeners.stderr.add(params.onStderr ?? console.warn);
    this.heartbeatTimeoutMs = params.heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
  }

  /**
   * Creates an iframe and sets up communication channels.
   * Returns after Elixir code sends `popcorn_elixir_ready` event.
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
    this.assertStatus(["uninitialized", "reload"]);
    this.transition({ status: "mount" });
    this.trace("Main: mount, container: ", this.bridgeConfig.container);

    this.bridge = new IframeBridge(this.bridgeConfig);

    try {
      const mountPromise = new Promise<void>((resolve) => {
        this.mountPromise = resolve;
      });

      await withTimeout(
        mountPromise.then(
          (): CallResult => ({
            ok: true,
            data: null,
            durationMs: 0,
          }),
        ),
        INIT_TIMEOUT_MS,
      );

      this.transition({ status: "ready" });
      this.trace("Main: mounted");
      this.onHeartbeat();
    } catch (error) {
      this.deinit();
      throw error;
    }
  }

  /**
   * Sends a message to an Elixir process and awaits for the response.
   *
   * If Elixir doesn't respond in configured timeout, the returned promise will be rejected with "process timeout" error.
   *
   * Unless passed via options, the default receiver registered via `Popcorn.Wasm.register_default_receiver/2` is used.
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
    { process, timeoutMs }: CallOptions = {},
  ): Promise<CallResult> {
    this.assertStatus(["ready"]);
    const targetProcess = process ?? this.defaultReceiver;
    if (this.bridge === null) throwError({ t: "unmounted" });
    if (targetProcess === null) throwError({ t: "bad_target" });

    const requestId = this.requestId++;
    const startTimeMs = performance.now();
    const callPromise = new Promise<CallResult>((resolve) => {
      if (this.bridge === null) throwError({ t: "unmounted" });
      this.trace("Main: call: ", { requestId, process, args });
      this.bridge.sendIframeRequest({
        type: MESSAGES.CALL,
        value: { requestId, process: targetProcess, args },
      });

      this.calls.set(requestId, {
        acknowledged: false,
        startTimeMs,
        resolve,
      });
    });

    const result = await withTimeout(callPromise, timeoutMs ?? CALL_TIMEOUT_MS);
    this.calls.delete(requestId);
    return result;
  }

  /**
   * Sends a message to an Elixir process (default or from options) and returns immediately.
   *
   * Unless passed via options, the default receiver registered via `Popcorn.Wasm.register_default_receiver/2` is used.
   * Throws "Unspecified target process" if default process is not set and no process is specified.
   */
  cast(args: AnySerializable, { process }: CastOptions = {}): void {
    this.assertStatus(["ready"]);
    const targetProcess = process ?? this.defaultReceiver;
    if (this.bridge === null) throwError({ t: "unmounted" });
    if (targetProcess === null) throwError({ t: "bad_target" });

    const requestId = this.requestId++;
    this.trace("Main: cast: ", { requestId, process, args });
    this.bridge.sendIframeRequest({
      type: MESSAGES.CAST,
      value: { requestId, process: targetProcess, args },
    });
  }

  /**
   * Registers an event handler function to receive events from Elixir.
   *
   * If this is the first handler registered, it will immediately process any events
   * that were received before any handlers were registered.
   */
  registerEventHandler(handler: EventHandler): void {
    const firstRegister = this.eventHandlers.length === 0;
    this.eventHandlers.push(handler);

    if (firstRegister) {
      this.processPreMountEvents();
    }
  }

  /**
   * Sets the default receiver process name for subsequent calls.
   */
  setDefaultReceiver(processName: string): void {
    this.defaultReceiver = processName;
  }

  /**
   * Destroys an iframe and resets the instance.
   */
  deinit() {
    if (this.bridge === null) throwError({ t: "unmounted" });
    this.trace("Main: deinit");
    this.transition({ status: "deinit" });
    this.bridge.deinit();
    this.bridge = null;
    if (this.heartbeatTimeout) {
      clearTimeout(this.heartbeatTimeout);
      this.heartbeatTimeout = null;
    }
    this.logListeners.stdout.clear();
    this.logListeners.stderr.clear();
    for (const callData of this.calls.values()) {
      const durationMs = performance.now() - callData.startTimeMs;
      callData.resolve({
        ok: false,
        error: new PopcornError("deinitialized"),
        durationMs,
      });
    }
    this.calls.clear();
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

  private iframeHandler(data: IframeResponse) {
    if (data.type === MESSAGES.STDOUT) {
      this.notifyLogListeners("stdout", data.value);
    } else if (data.type === MESSAGES.STDERR) {
      this.notifyLogListeners("stderr", data.value);
    } else if (data.type === MESSAGES.CALL) {
      this.onCall(data.value);
    } else if (data.type === MESSAGES.CALL_ACK) {
      this.onCallAck(data.value);
    } else if (data.type === MESSAGES.EVENT) {
      this.onEvent(data.value);
    } else if (data.type === MESSAGES.HEARTBEAT) {
      this.onHeartbeat();
    } else if (data.type === MESSAGES.RELOAD) {
      this.reloadIframe();
    } else {
      throwError({ t: "assert" });
    }
  }

  private dispatchPopcornEvent(eventName: string, payload: AnySerializable): void {
    switch (eventName) {
      case EVENT_NAMES.ELIXIR_READY:
        if (this.mountPromise) {
          this.mountPromise();
          this.mountPromise = null;
        }
        this.preMountEvents.push({ eventName, payload });
        break;
      case EVENT_NAMES.SET_DEFAULT_RECEIVER:
        this.setDefaultReceiver(payload.name);
        break;
      default:
        console.warn(`Received unhandled popcorn event: ${eventName}`);
    }
  }

  private onEvent({ eventName, payload }: ElixirEvent): void {
    this.trace("Main: onEvent: ", { eventName, payload });

    if (eventName.startsWith("popcorn")) {
      this.dispatchPopcornEvent(eventName, payload);
      return;
    }

    if (this.mountPromise !== null) {
      this.preMountEvents.push({ eventName, payload });
      return;
    }

    this.dispatchEvent(eventName, payload);
  }

  private dispatchEvent(eventName: string, payload: AnySerializable): void {
    this.trace("Main: dispatching event: ", { eventName, payload });

    for (const eventHandler of this.eventHandlers) {
      try {
        eventHandler(eventName, payload);
      } catch (error) {
        console.error(
          `Error in global event handler for '${eventName}':`,
          error,
        );
      }
    }
  }

  private processPreMountEvents(): void {
    this.trace(
      "Main: processing event queue, length: ",
      this.preMountEvents.length,
    );

    for (const { eventName, payload } of this.preMountEvents) {
      this.dispatchEvent(eventName, payload);
    }

    this.preMountEvents = [];
  }

  private onCallAck({ requestId }: { requestId: number }): void {
    this.assertStatus(["ready"]);
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
    this.assertStatus(["ready"]);
    this.trace("Main: onCall: ", { requestId, error, data });
    const callData = this.calls.get(requestId);
    if (callData === undefined) throwError({ t: "bad_call" });
    if (!callData.acknowledged) throwError({ t: "no_acked_call" });

    this.calls.delete(requestId);

    const durationMs = performance.now() - callData.startTimeMs;
    if (error !== undefined) {
      callData.resolve({ ok: false, error, durationMs });
    } else {
      callData.resolve({ ok: true, data, durationMs });
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

  private reloadIframe(reason = "other"): void {
    if (this.bridge === null) {
      throwError({ t: "unmounted" });
    }

    if (document.hidden) {
      this.trace("Main: reloading iframe skipped, window not visible");
      return;
    }

    this.reloadN++;
    if (this.reloadN > MAX_RELOAD_N) {
      this.trace("Main: exceeded max reload number");
      return;
    }

    this.trace("Main: reloading iframe");
    this.transition({ status: "reload" });
    this.bridge.deinit();
    this.bridge = null;
    if (this.heartbeatTimeout) {
      clearTimeout(this.heartbeatTimeout);
      this.heartbeatTimeout = null;
    }
    for (const callData of this.calls.values()) {
      const durationMs = performance.now() - callData.startTimeMs;
      callData.resolve({
        ok: false,
        error: new PopcornError("reload"),
        durationMs,
      });
    }
    this.calls.clear();
    this.onReloadCallback(reason);
    this.mount();
  }

  trace(...messages: unknown[]): void {
    if (this.debug) {
      console.debug(...messages);
    }
  }

  private transition(to: State): void {
    this.trace(`State: ${this.state.status} -> ${to.status}`);
    this.state = to;
  }

  private assertStatus(validStatuses: State["status"][]): void {
    const currentStatus = this.state.status;
    if (!validStatuses.includes(currentStatus)) {
      throwError({
        t: "bad_status",
        status: currentStatus,
        expectedStatus: validStatuses.join(" | "),
      });
    }
  }
}

async function withTimeout(
  promise: Promise<CallResult>,
  ms: number,
): Promise<CallResult> {
  let timeout: ReturnType<typeof setTimeout> | null = null;
  const timeoutPromise = new Promise<CallResult>((resolve) => {
    timeout = setTimeout(() => {
      resolve({
        ok: false,
        error: new PopcornError("timeout"),
        durationMs: ms,
      });
    }, ms);
  });

  const result = await Promise.race([promise, timeoutPromise]);

  if (!timeout) throwError({ t: "assert" });
  clearTimeout(timeout);
  return result;
}

function noop() {
  /* noop */
}
