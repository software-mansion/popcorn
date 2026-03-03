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

/** @module popcorn */

/**
 * @typedef {Object} CallResult
 * @property {AnySerializable} data Serialized value returned from Elixir
 * @property {number} durationMs Amount of time it took to process the call
 * @property {AnySerializable} [error] Optional error if call failed
 */

/** @typedef {any} AnySerializable */

class PopcornDeinitializedError extends Error { }

/**
 * Manages Elixir by setting up iframe, WASM module, and event listeners. Used to sent messages to Elixir processes.
 */
export class Popcorn {
  onStdout = null;
  onStderr = null;
  heartbeatTimeoutMs = null;

  _debug = false;
  _bundlePath = null;
  _wasmDir = null;
  _initProcess = null;

  _requestId = 0;
  _calls = new Map();
  _listenerRef = null;
  _logListeners = { stdout: new Set(), stderr: new Set() };

  _iframe = null;
  _awaitedMessage = null;
  _heartbeatTimeout = null;

  /** @hideconstructor */
  constructor(params, token) {
    if (token !== INIT_TOKEN) {
      throw new Error(
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    }
    const { bundlePath, onStderr, onStdout, heartbeatTimeoutMs, debug, wasmDir } =
      params;

    this._debug = debug;
    this._bundlePath = bundlePath ?? "wasm/bundle.avm";
    this.onStdout = onStdout ?? console.log;
    this.onStderr = onStderr ?? console.warn;
    this.heartbeatTimeoutMs = heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
    this._wasmDir = wasmDir ?? "./wasm/";
  }

  /**
   * Creates an iframe and sets up communication channels.
   * Returns after Elixir code calls `Popcorn.Wasm.register/1`.
   *
   * @param {object} options used to set timeouts, event handlers, path to bundle, etc.
   * @param {HTMLElement} [options.container=document.body] DOM element to mount an iframe
   * @param {string} [options.bundlePath="wasm/bundle.avm"] Path to compiled Elixir bundle (`.avm` file).
   * @param {function(string): void} [options.onStderr=noop] Handler for stdout messages.
   * @param {function(string): void} [options.onStdout=noop] Handler for stderr messages.
   * @param {number} [options.heartbeatTimeoutMs=15_000] Heartbeat timeout in milliseconds. If an iframe doesn't respond within this time, it is reloaded.
   * @param {string} [options.wasmDir="./wasm/"] - Directory containing Wasm and scripts used inside iframe.
   * @param {boolean} [options.debug=false] Enable debug logging.
   * @example
   * import { Popcorn } from "./wasm/popcorn.js";

   * const popcorn = await Popcorn.init({
   *   onStdout: console.log,
   *   onStderr: console.error,
   *   debug: true,
   * });
   * @returns {Promise<Popcorn>} Popcorn instance.
   */
  static async init(options) {
    const { container, ...constructorParams } = options;
    const popcorn = new Popcorn(constructorParams, INIT_TOKEN);
    popcorn._trace("Main: init, params: ", { container, ...constructorParams });
    await popcorn._mount(container ?? document.documentElement);
    return popcorn;
  }

  async _mount(container) {
    if (this._iframe !== null) {
      throw new Error("Iframe already mounted");
    }

    this._trace("Main: mount, container: ", container);

    this._iframe = document.createElement("iframe");
    this._iframe.srcdoc = `<html>
      <html lang="en" dir="ltr">
          <head>
            <meta name="bundle-path" content="${"../" + this._bundlePath}" />
          </head>
          <script type="module" src="${this._wasmDir + "popcorn.js"}" defer></script>
          <script type="module" src="${this._wasmDir + "AtomVM.mjs"}" defer></script>
          <script type="module" src="${this._wasmDir + "popcorn_iframe.js"}" defer></script>
          <script type="module" defer>
            import { runIFrame } from "${this._wasmDir + "popcorn_iframe.js"}";
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
    this._initProcess = await withTimeout(this._awaitMessage(MESSAGES.START_VM), INIT_VM_TIMEOUT_MS);
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
   * @param {AnySerializable} args serializable data sent to Elixir process.
   * @param {object} options
   * @param {string} [options.process] Registered Elixir process name.
   * @param {number} [options.timeoutMs=60_000] Timeout (in milliseconds) for the call
   * @example
   * const result = await popcorn.call(
   *   { action: "get_user", id: 123 },
   *   { process: "user_server", timeoutMs: 5_000 },
   * );
   * console.log(result.data); // Deserialized Elixir response
   * console.log(result.durationMs); // Entire call duration
   * @returns {Promise<CallResult>} A promise resolved with {@link CallResult} or rejected on timeout
   */
  async call(args, { process, timeoutMs }) {
    const targetProcess = process ?? this._initProcess;
    if (this._iframe === null) {
      throw new Error("WASM iframe not mounted");
    }
    if (targetProcess === null) {
      throw new Error("Unspecified target process");
    }

    const requestId = this._requestId++;
    const callPromise = new Promise((resolve, reject) => {
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
   *
   * @param {AnySerializable} args sent to Elixir process.
   * @param {object} options options for cast
   * @param {string} [options.process] receiver process name.
   */
  cast(args, { process }) {
    const targetProcess = process ?? this._initProcess;
    if (this._iframe === null) {
      throw new Error("WASM iframe not mounted");
    }
    if (targetProcess === null) {
      throw new Error("Unspecified target process");
    }

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
    window.removeEventListener("message", this._listenerRef);
    this._iframe.remove();
    this._iframe = null;
    this._awaitedMessage = null;
    this._listenerRef = null;
    this._logListeners.stdout.clear();
    this._logListeners.stderr.clear();
    clearTimeout(this._heartbeatTimeout);
    for (const [_id, callData] of this._calls) {
      const durationMs = performance.now() - callData.startTimeMs;
      callData.reject({ error: new PopcornDeinitializedError("Call cancelled due to instance deinit"), durationMs })
    }
  }

  /**
   * Registers a log listener that will be called when output of the specified type is received.
   * @param {function(string): void} listener - Callback function that receives the log message
   * @param {"stdout" | "stderr"} type - The output type to listen for
   */
  registerLogListener(listener, type) {
    if (type !== "stdout" && type !== "stderr") {
      throw new Error(`Invalid output type: ${type}. Must be "stdout" or "stderr"`);
    }
    this._logListeners[type].add(listener);
  }

  /**
   * Unregisters a previously registered log listener.
   * @param {function(string): void} listener - Callback function to remove
   * @param {"stdout" | "stderr"} type - The output type the listener was registered for
   */
  unregisterLogListener(listener, type) {
    if (type !== "stdout" && type !== "stderr") {
      throw new Error(`Invalid output type: ${type}. Must be "stdout" or "stderr"`);
    }
    this._logListeners[type].delete(listener);
  }

  _notifyLogListeners(type, message) {
    this._logListeners[type].forEach((listener) => {
      listener(message);
    });
  }

  _iframeListener({ data }) {
    const awaitedMessage = this._awaitedMessage;
    if (awaitedMessage && data.type == awaitedMessage.type) {
      this._awaitedMessage = null;
      awaitedMessage.resolve(data.value);
      return;
    }

    const handlers = {
      [MESSAGES.STDOUT]: (value) => {
        this.onStdout(value);
        this._notifyLogListeners("stdout", value);
      },
      [MESSAGES.STDERR]: (value) => {
        this.onStderr(value);
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

  _onCallAck({ requestId }) {
    this._trace("Main: onCallAck: ", { requestId });
    const callData = this._calls.get(requestId);
    if (callData === undefined) {
      throw new Error("Ack for non-existent call");
    }
    this._calls.set(requestId, { ...callData, acknowledged: true });
  }

  _onCall({ requestId, error, data }) {
    this._trace("Main: onCall: ", { requestId, error, data });
    const callData = this._calls.get(requestId);
    if (callData === undefined) {
      throw new Error("Response for non-existent call");
    }
    if (!callData.acknowledged) {
      throw new Error("Response for non-acknowledged call");
    }

    this._calls.delete(requestId);

    const durationMs = performance.now() - callData.startTimeMs;
    if (error !== undefined) {
      callData.reject({ error, durationMs });
    } else {
      callData.resolve({ data, durationMs });
    }
  }

  _onHeartbeat() {
    clearTimeout(this._heartbeatTimeout);
    this._heartbeatTimeout = setTimeout(() => {
      this._trace("Main: heartbeat lost");
      this._reloadIframe("heartbeat_lost");
    }, this.heartbeatTimeoutMs);
  }

  _reloadIframe(reason = "other") {
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
    this._mount(container);
  }

  _send(type, data) {
    this._iframe.contentWindow.postMessage({ type, value: data });
  }

  _awaitMessage(type) {
    if (this._awaitedMessage) {
      throw new Error(`Cannot await message ${this._awaitedMessage.type} when a message ${type} is already awaited on`)
    }

    this._awaitedMessage = { type };

    return new Promise((resolve) => {
      this._awaitedMessage.resolve = resolve;
    })
  }

  _trace(...messages) {
    if (this._debug) {
      console.debug(...messages);
    }
  }
}

async function withTimeout(promise, ms) {
  let timeout = null;
  const timeoutPromise = new Promise((_resolve, reject) => {
    timeout = setTimeout(() => {
      reject("Promise timeout");
    }, ms);
  });

  const result = await Promise.race([promise, timeoutPromise]);
  clearTimeout(timeout);
  return result;
}
