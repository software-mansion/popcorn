const INIT_TIMEOUT_MS = 1_000;
const CALL_TIMEOUT_MS = 5_000;
const HEARTBEAT_TIMEOUT_MS = 15_000;

// postMessage data:
//  | { type: "stdout", value: string }
//  | { type: "stderr", value: string }
//  | { type: "init", value: string }
//  | { type: "call", value: ElixirRequest | ElixirResponse}
//  | { type: "callAck", value: ElixirAck }
//  | { type: "cast", value: ElixirRequest }
//  | { type: "heartbeat", value: void }
//
// ElixirRequest: { requestId: number, process: string, action: string, args: any }
// ElixirResponse: { requestId: number, error: string } | { requestId: number, data: any }
// ElixirAck: { requestId: number }
const MESSAGES = {
  INIT: "fission-init",
  CALL: "fission-call",
  CAST: "fission-cast",
  CALL_ACK: "fission-callAck",
  STDOUT: "fission-stdout",
  STDERR: "fission-stderr",
  HEARTBEAT: "fission-heartbeat",
};

const INIT_TOKEN = Symbol();

export class Fission {
  onStdout = null;
  onStderr = null;
  heartbeatTimeoutMs = null;

  _debug = false;
  _bundleName = null;
  _initProcess = null;

  _requestId = 0;
  _calls = new Map();
  _listenerRef = null;

  _iframe = null;
  _mountPromise = null;
  _heartbeatTimeout = null;

  constructor(params, token) {
    if (token !== INIT_TOKEN) {
      throw new Error(
        "Don't construct object directly, use Fission.init() instead",
      );
    }
    const { bundleName, onStderr, onStdout, heartbeatTimeoutMs, debug } =
      params;
    if (bundleName === undefined) {
      throw new Error("bundleName is required");
    }

    this._debug = debug;
    this._bundleName = bundleName;
    this.onStdout = onStdout ?? noop;
    this.onStderr = onStderr ?? noop;
    this.heartbeatTimeoutMs = heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
  }

  get iframe() {
    return this._iframe;
  }

  static async init({ container, ...constructorParams }) {
    const fission = new Fission(constructorParams, INIT_TOKEN);
    await fission._mount(container);
    return fission;
  }

  async _mount(container) {
    if (this._iframe !== null) {
      throw new Error("Iframe already mounted");
    }

    const mountPromise = new Promise((resolve) => {
      // will be resolved on init message from iframe
      this._mountPromise = resolve;

      this._iframe = document.createElement("iframe");
      this._iframe.srcdoc = `<html>
        <html lang="en" dir="ltr">
            <head>
              <meta name="bundle-name" content="${this._bundleName}" />
            </head>
            <script type="module" src="fission.js" defer></script>
            <script type="module" src="AtomVM.mjs" defer></script>
            <script type="module" src="fission_iframe.js" defer></script>
            <script type="module" defer>
              import { initVm } from "./fission_iframe.js";
              initVm();
            </script>
        </html>`;
      this._iframe.style = "visibility: hidden;";

      // TODO: handle multiple iframes
      this._listenerRef = this._iframeListener.bind(this);
      window.addEventListener("message", this._listenerRef);
      container.appendChild(this._iframe);
    });

    await withTimeout(mountPromise, INIT_TIMEOUT_MS);
    // sucessfully mounted, kick-off heartbeat listener
    this._trace("Main: mount");
    this._onHeartbeat();
  }

  deinit() {
    if (this._iframe === null) {
      throw new Error("Iframe not mounted");
    }

    window.removeEventListener("message", this._listenerRef);
    this._iframe.remove();
    this._iframe = null;
    this._mountPromise = null;
    this._listenerRef = null;
  }

  // TODO: collapse action and args into one argument
  async call(action, args, { process, timeoutMs }) {
    const targetProcess = process ?? this._initProcess;
    if (this.iframe === null) {
      throw new Error("WASM iframe not mounted");
    }
    if (targetProcess === null) {
      throw new Error("Unspecified target process");
    }

    const requestId = this._requestId++;
    const callPromise = new Promise((resolve, reject) => {
      this._send(MESSAGES.CALL, {
        requestId,
        process: targetProcess,
        action,
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

  // TODO: collapse action and args into one argument
  cast(action, args, { process }) {
    const targetProcess = process ?? this._initProcess;
    if (this.iframe === null) {
      throw new Error("WASM iframe not mounted");
    }
    if (targetProcess === null) {
      throw new Error("Unspecified target process");
    }

    const requestId = this._requestId++;
    this._send(MESSAGES.CAST, {
      requestId,
      process: targetProcess,
      action,
      args,
    });
  }

  _iframeListener({ data }) {
    const handlers = {
      [MESSAGES.STDOUT]: this.onStdout.bind(this),
      [MESSAGES.STDERR]: this.onStderr.bind(this),
      [MESSAGES.INIT]: this._onInit.bind(this),
      [MESSAGES.CALL]: this._onCall.bind(this),
      [MESSAGES.CALL_ACK]: this._onCallAck.bind(this),
      [MESSAGES.HEARTBEAT]: this._onHeartbeat.bind(this),
    };

    const handler = handlers[data.type];
    if (handler !== undefined) {
      handler(data.value);
    } else if (data.type?.startsWith("fission")) {
      console.warn(
        `Received unhandled event: ${JSON.stringify(data, null, 4)}`,
      );
    }
  }

  _onInit(initProcess) {
    this._mountPromise();
    this._initProcess = initProcess;
  }

  _onCallAck({ requestId }) {
    const callData = this._calls.get(requestId);
    if (callData === undefined) {
      throw new Error("Ack for non-existent call");
    }
    this._calls.set(requestId, { ...callData, acknowledged: true });
  }

  _onCall({ requestId, error, data }) {
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
      this._trace("Main: hearbeat lost");
      this._reloadIframe();
    }, this.heartbeatTimeoutMs);
  }

  _reloadIframe() {
    this._trace("Main: reload iframe");
    if (this._iframe === null) {
      throw new Error("WASM iframe not mounted for reload");
    }

    this.deinit();
    const container = this._iframe.parentElement;
    this._mount(container);
  }

  _send(type, data) {
    this._iframe.contentWindow.postMessage({ type, value: data });
  }

  _trace(message) {
    if (this._debug) {
      console.trace(message);
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
