const INIT_TIMEOUT_MS = 30_000;
const CALL_TIMEOUT_MS = 60_000;
const HEARTBEAT_TIMEOUT_MS = 60_000;

// postMessage data:
//  | { type: "stdout", value: string }
//  | { type: "stderr", value: string }
//  | { type: "call", value: ElixirRequest | ElixirResponse}
//  | { type: "callAck", value: ElixirAck }
//  | { type: "cast", value: ElixirRequest }
//  | { type: "event", value: ElixirEvent }
//  | { type: "heartbeat", value: void }
//  | { type: "reload", value: void }
//
// ElixirRequest: { requestId: number, process: string, action: string, args: any }
// ElixirResponse: { requestId: number, error: string } | { requestId: number, data: any }
// ElixirAck: { requestId: number }
// ElixirEvent: { eventName: string, payload: any }
const MESSAGES = {
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  EVENT: "popcorn-event",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
  RELOAD: "popcorn-reload",
};

const EVENT_NAMES = {
  ELIXIR_READY: "popcorn_elixir_ready",
  SET_DEFAULT_RECEIVER: "popcorn_set_default_receiver",
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

class PopcornDeinitializedError extends Error {}

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

  _requestId = 0;
  _calls = new Map();
  _listenerRef = null;

  _iframe = null;
  _mountPromise = null;
  _heartbeatTimeout = null;

  _eventHandlers = [];
  _preMountEvents = [];
  _defaultReceiver = null;

  /** @hideconstructor */
  constructor(params, token) {
    if (token !== INIT_TOKEN) {
      throw new Error(
        "Don't construct the Popcorn object directly, use Popcorn.init() instead"
      );
    }
    const {
      bundlePath,
      onStderr,
      onStdout,
      heartbeatTimeoutMs,
      debug,
      wasmDir,
    } = params;

    this._debug = debug;
    this._bundlePath = bundlePath ?? "wasm/bundle.avm";
    this.onStdout = onStdout ?? console.log;
    this.onStderr = onStderr ?? console.warn;
    this.heartbeatTimeoutMs = heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
    this._wasmDir = wasmDir ?? "./wasm/";
  }

  /**
   * Creates an iframe and sets up communication channels.
   * Returns after Elixir code calls `Popcorn.Wasm.send_elixir_ready/0`.
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
    const mountPromise = new Promise((resolve) => {
      // will be resolved on elixir ready event

      this._mountPromise = resolve;

      // // example pathname: "/wasm/popcorn.js"
      this._iframe = document.createElement("iframe");
      this._iframe.srcdoc = `<html>
        <html lang="en" dir="ltr">
            <head>
              <meta name="bundle-path" content="${"../" + this._bundlePath}" />
            </head>
            <script type="module" src="${
              this._wasmDir + "popcorn.js"
            }" defer></script>
            <script type="module" src="${
              this._wasmDir + "AtomVM.mjs"
            }" defer></script>
            <script type="module" src="${
              this._wasmDir + "popcorn_iframe.js"
            }" defer></script>
            <script type="module" defer>
              import { initVm } from "${this._wasmDir + "popcorn_iframe.js"}";
              initVm();
            </script>
        </html>`;
      this._iframe.style =
        "visibility: hidden; width: 0px; height: 0px; border: none";

      // TODO: handle multiple iframes
      this._listenerRef = this._iframeListener.bind(this);
      window.addEventListener("message", this._listenerRef);
      container.appendChild(this._iframe);
    });

    await withTimeout(mountPromise, INIT_TIMEOUT_MS);
    // successfully mounted, kick-off heartbeat listener
    this._trace("Main: mount");
    this._onHeartbeat();
  }

  /**
   * Sends a message to an Elixir process and awaits for the response.
   *
   * If Elixir doesn't respond in configured timeout, the returned promise will be rejected with "process timeout" error.
   *
   * Unless passed via options, the default receiver registered via `Popcorn.Wasm.register_default_receiver/2` is used.
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
    const targetProcess = process ?? this._defaultReceiver;
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
   * Registers an event handler function to receive events from Elixir.
   *
   * If this is the first handler registered, it will immediately process any events
   * that were received before any handlers were registered.
   *
   * @param {function(string, AnySerializable): void} handler - Function that receives event name and payload
   * @example
   * popcorn.registerEventHandler((eventName, payload) => {
   *   console.log(`Received event: ${eventName}`, payload);
   *   if (eventName === "user_logged_in") {
   *     updateUI(payload.user);
   *   }
   * });
   */
  registerEventHandler(handler) {
    const firstRegister = this._eventHandlers.length === 0;
    this._eventHandlers.push(handler);

    if (firstRegister) {
      this._processPreMountEvents();
    }
  }

  /**
   * Sends a message to an Elixir process (default or from options) and returns immediately.
   *
   * Unless passed via options, the default receiver registered via `Popcorn.Wasm.register_default_receiver/2` is used.
   * Throws "Unspecified target process" if default process is not set and no process is specified.
   *
   * @param {AnySerializable} args sent to Elixir process.
   * @param {object} options options for cast
   * @param {string} [options.process] receiver process name.
   */
  cast(args, { process }) {
    const targetProcess = process ?? this._defaultReceiver;
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
   * Sets the default receiver process name for subsequent calls.
   *
   * @param {string} processName - The name of the process to use as default receiver
   */
  setDefaultReceiver(processName) {
    this._defaultReceiver = processName;
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
    this._listenerRef = null;
    clearTimeout(this._heartbeatTimeout);
    for (const [_id, callData] of this._calls) {
      const durationMs = performance.now() - callData.startTimeMs;
      callData.reject({
        error: new PopcornDeinitializedError(
          "Call cancelled due to instance deinit"
        ),
        durationMs,
      });
    }
  }

  _iframeListener({ data }) {
    const handlers = {
      [MESSAGES.STDOUT]: this.onStdout.bind(this),
      [MESSAGES.STDERR]: this.onStderr.bind(this),
      [MESSAGES.CALL]: this._onCall.bind(this),
      [MESSAGES.CALL_ACK]: this._onCallAck.bind(this),
      [MESSAGES.EVENT]: this._onEvent.bind(this),
      [MESSAGES.HEARTBEAT]: this._onHeartbeat.bind(this),
      [MESSAGES.RELOAD]: this._reloadIframe.bind(this),
    };

    const handler = handlers[data.type];
    if (handler !== undefined) {
      handler(data.value);
    } else if (data.type?.startsWith("popcorn")) {
      console.warn(
        `Received unhandled event: ${JSON.stringify(data, null, 4)}`
      );
    }
  }

  _dispatchPopcornEvent(eventName, payload) {
    switch (eventName) {
      case EVENT_NAMES.ELIXIR_READY:
        if (this._mountPromise) {
          this._mountPromise();
          this._mountPromise = null;
        }
        this._preMountEvents.push({ eventName, payload });
        break;
      case EVENT_NAMES.SET_DEFAULT_RECEIVER:
        this.setDefaultReceiver(payload.name);
        break;
      default:
        console.warn(`Received unhandled popcorn event: ${eventName}`);
    }
  }

  _onEvent({ eventName, payload }) {
    this._trace("Main: onEvent: ", { eventName, payload });

    if (eventName.startsWith("popcorn")) {
      this._dispatchPopcornEvent(eventName, payload);
      return;
    }

    if (this._mountPromise !== null) {
      this._preMountEvents.push({ eventName, payload });
      return;
    }

    this._dispatchEvent(eventName, payload);
  }

  _dispatchEvent(eventName, payload) {
    this._trace("Main: dispatching event: ", { eventName, payload });

    for (const eventHandler of this._eventHandlers) {
      try {
        eventHandler(eventName, payload);
      } catch (error) {
        console.error(
          `Error in global event handler for '${eventName}':`,
          error
        );
      }
    }
  }

  _processPreMountEvents() {
    this._trace(
      "Main: processing event queue, length: ",
      this._preMountEvents.length
    );

    for (const { eventName, payload } of this._preMountEvents) {
      this._dispatchEvent(eventName, payload);
    }

    this._preMountEvents = [];
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

  _trace(...messages) {
    if (this._debug) {
      console.debug(...messages);
    }
  }
}

function noop() {}

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
