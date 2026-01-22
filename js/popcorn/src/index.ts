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
};

const INIT_TOKEN = Symbol();

type CallResult = {
  /** Serialized value returned from Elixir */
  data: AnySerializable;
  /** Amount of time it took to process the call */
  durationMs: number;
  /** Optional error if call failed */
  error?: AnySerializable;
};

// eslint-disable-next-line @typescript-eslint/no-explicit-any
type AnySerializable = any;

/** Output type for log listeners */
type LogType = "stdout" | "stderr";

/** Callback function that receives the log message */
type LogListener = (message: string) => void;

/** Options for Popcorn.init() */
type PopcornInitOptions = {
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

/** Options for call method */
type CallOptions = {
  /** Registered Elixir process name. */
  process?: string;
  /** Timeout (in milliseconds) for the call */
  timeoutMs?: number;
};

/** Options for cast method */
type CastOptions = {
  /** Receiver process name. */
  process?: string;
};

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

/**
 * Manages Elixir by setting up iframe, WASM module, and event listeners. Used to sent messages to Elixir processes.
 */
export class Popcorn {
  onStdout: ((message: string) => void) | null = null;
  onStderr: ((message: string) => void) | null = null;
  heartbeatTimeoutMs: number | null = null;

  _debug = false;
  _bundleURL: string | null = null;
  _initProcess: string | null = null;

  _requestId = 0;
  _calls = new Map<number, CallData>();
  _listenerRef: ((event: MessageEvent) => void) | null = null;
  _logListeners: { stdout: Set<LogListener>; stderr: Set<LogListener> } = {
    stdout: new Set(),
    stderr: new Set(),
  };

  _iframe: HTMLIFrameElement | null = null;
  _awaitedMessage: AwaitedMessage | null = null;
  _heartbeatTimeout: ReturnType<typeof setTimeout> | null = null;

  /** @hideconstructor */
  constructor(params: Omit<PopcornInitOptions, "container">, token: symbol) {
    if (token !== INIT_TOKEN) {
      throw new Error(
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    }
    const { bundlePath, onStderr, onStdout, heartbeatTimeoutMs, debug } =
      params;

    this._debug = debug ?? false;
    this._bundleURL = new URL(
      bundlePath ?? "/bundle.avm",
      import.meta.url,
    ).href;
    this.onStdout = onStdout ?? console.log;
    this.onStderr = onStderr ?? console.warn;
    this.heartbeatTimeoutMs = heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
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
    const popcorn = new Popcorn(constructorParams, INIT_TOKEN);
    popcorn._trace("Main: init, params: ", { container, ...constructorParams });
    await popcorn._mount(container ?? document.documentElement);
    return popcorn;
  }

  async _mount(container: HTMLElement): Promise<void> {
    if (this._iframe !== null) {
      throw new Error("Iframe already mounted");
    }

    this._trace("Main: mount, container: ", container);

    const iframeUrl = new URL("./iframe.mjs", import.meta.url).href;

    this._iframe = document.createElement("iframe");
    this._iframe.srcdoc = `<html>
      <html lang="en" dir="ltr">
          <head>
            <meta name="bundle-path" content="${this._bundleURL}" />
          </head>
          <script type="module" defer>
            import { runIFrame } from "${iframeUrl}";
            runIFrame();
          </script>
      </html>`;
    this._iframe.style =
      "visibility: hidden; width: 0px; height: 0px; border: none";

    // TODO: handle multiple iframes
    this._listenerRef = this._iframeListener.bind(this);
    window.addEventListener("message", this._listenerRef);
    container.appendChild(this._iframe);
    await this._awaitMessage(MESSAGES.INIT);
    this._trace("Main: iframe loaded");
    this._initProcess = await withTimeout(
      this._awaitMessage(MESSAGES.START_VM),
      INIT_VM_TIMEOUT_MS,
    );
    this._trace("Main: mounted, main process: ", this._initProcess);
    this._onHeartbeat();
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
    const targetProcess = process ?? this._initProcess;
    if (this._iframe === null) throwError({ t: "unmounted" });
    if (targetProcess === null) throwError({ t: "bad_target" });

    const requestId = this._requestId++;
    const callPromise = new Promise<CallResult>((resolve, reject) => {
      this._trace("Main: call: ", { requestId, process, args });
      this._send(MESSAGES.CALL, {
        requestId,
        process: targetProcess,
        args,
      });

      this._calls.set(requestId, {
        acknowledged: false,
        startTimeMs: performance.now(),
        resolve,
        reject,
      });
    });

    const result = await withTimeout(callPromise, timeoutMs ?? CALL_TIMEOUT_MS);
    this._calls.delete(requestId);
    return result;
  }

  /**
   * Sends a message to an Elixir process (default or from options) and returns immediately.
   *
   * Unless passed via options, the name passed in `Popcorn.Wasm.register/1` on the Elixir side is used.
   * Throws "Unspecified target process" if default process is not set and no process is specified.
   */
  cast(args: AnySerializable, { process }: CastOptions): void {
    const targetProcess = process ?? this._initProcess;
    if (this._iframe === null) throwError({ t: "unmounted" });
    if (targetProcess === null) throwError({ t: "bad_target" });

    const requestId = this._requestId++;
    this._trace("Main: cast: ", { requestId, process, args });
    this._send(MESSAGES.CAST, {
      requestId,
      process: targetProcess,
      args,
    });
  }

  /**
   * Destroys an iframe and resets the instance.
   */
  deinit() {
    if (this._iframe === null) {
      throw new Error("Iframe not mounted");
    }

    this._trace("Main: deinit");
    if (!this._listenerRef) throwError({ t: "assert" });
    window.removeEventListener("message", this._listenerRef);
    this._iframe.remove();
    this._iframe = null;
    this._awaitedMessage = null;
    this._listenerRef = null;
    this._logListeners.stdout.clear();
    this._logListeners.stderr.clear();
    for (const callData of this._calls.values()) {
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
    this._logListeners[type].add(listener);
  }

  /**
   * Unregisters a previously registered log listener.
   */
  unregisterLogListener(listener: LogListener, type: LogType): void {
    this._logListeners[type].delete(listener);
  }

  _notifyLogListeners(type: LogType, message: string): void {
    this._logListeners[type].forEach((listener) => {
      listener(message);
    });
  }

  _iframeListener({ data }: MessageEvent): void {
    const awaitedMessage = this._awaitedMessage;
    if (awaitedMessage && data.type == awaitedMessage.type) {
      this._awaitedMessage = null;
      awaitedMessage.resolve?.(data.value);
      return;
    }

    const handlers: Record<string, (value: AnySerializable) => void> = {
      [MESSAGES.STDOUT]: (value: string) => {
        this.onStdout?.(value);
        this._notifyLogListeners("stdout", value);
      },
      [MESSAGES.STDERR]: (value: string) => {
        this.onStderr?.(value);
        this._notifyLogListeners("stderr", value);
      },
      [MESSAGES.CALL]: this._onCall.bind(this),
      [MESSAGES.CALL_ACK]: this._onCallAck.bind(this),
      [MESSAGES.HEARTBEAT]: this._onHeartbeat.bind(this),
      [MESSAGES.RELOAD]: this._reloadIframe.bind(this),
    };

    const handler = handlers[data.type];
    if (handler !== undefined) {
      handler(data.value);
    } else if (
      typeof data.type === "string" &&
      data.type?.startsWith("popcorn")
    ) {
      console.warn(
        `Received unhandled event: ${JSON.stringify(data, null, 4)}`,
      );
    }
  }

  _onCallAck({ requestId }: { requestId: number }): void {
    this._trace("Main: onCallAck: ", { requestId });
    const callData = this._calls.get(requestId);
    if (callData === undefined) throwError({ t: "bad_ack" });

    this._calls.set(requestId, { ...callData, acknowledged: true });
  }

  _onCall({
    requestId,
    error,
    data,
  }: {
    requestId: number;
    error?: AnySerializable;
    data?: AnySerializable;
  }): void {
    this._trace("Main: onCall: ", { requestId, error, data });
    const callData = this._calls.get(requestId);
    if (callData === undefined) throwError({ t: "bad_call" });
    if (!callData.acknowledged) throwError({ t: "no_acked_call" });

    this._calls.delete(requestId);

    const durationMs = performance.now() - callData.startTimeMs;
    if (error !== undefined) {
      callData.reject({ error, durationMs });
    } else {
      callData.resolve({ data, durationMs });
    }
  }

  _onHeartbeat(): void {
    if (this._heartbeatTimeout) {
      clearTimeout(this._heartbeatTimeout);
    }
    this._heartbeatTimeout = setTimeout(() => {
      this._trace("Main: heartbeat lost");
      this._reloadIframe("heartbeat_lost");
      // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    }, this.heartbeatTimeoutMs!);
  }

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  _reloadIframe(_reason = "other"): void {
    if (this._iframe === null) {
      throw new Error("WASM iframe not mounted for reload");
    }

    if (document.hidden) {
      this._trace("Main: reloading iframe skipped, window not visible");
      return;
    }
    this._trace("Main: reloading iframe");
    const container = this._iframe.parentElement;
    this.deinit();
    if (container) {
      this._mount(container);
    }
  }

  _send(type: string, data: AnySerializable): void {
    this._iframe?.contentWindow?.postMessage({ type, value: data });
  }

  _awaitMessage(type: string): Promise<AnySerializable> {
    if (this._awaitedMessage) {
      throwError({
        t: "already_awaited",
        messageType: this._awaitedMessage.type,
        awaitedMessageType: type,
      });
    }

    this._awaitedMessage = { type };

    return new Promise((resolve) => {
      if (!this._awaitedMessage) throwError({ t: "assert" });
      this._awaitedMessage.resolve = resolve;
    });
  }

  _trace(...messages: unknown[]): void {
    if (this._debug) {
      console.debug(...messages);
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
  | { t: "bad_call" }
  | { t: "no_acked_call" }
  | { t: "bad_ack" }
  | { t: "unmounted" }
  | { t: "bad_target" }
  | {
      t: "already_awaited";
      messageType: string;
      awaitedMessageType: string;
    };

function throwError(error: ErrorData): never {
  switch (error.t) {
    case "assert":
      throw new Error("Assertion error");
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
  }
}
