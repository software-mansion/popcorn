const INIT_TIMEOUT_MS = 5_000;
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
  INIT: "popcorn-init",
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
};

const INIT_TOKEN = Symbol();

export class Popcorn {
  onStdout = null;
  onStderr = null;
  heartbeatTimeoutMs = null;

  _debug = false;
  _bundlePath = null;
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
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    }
    const { bundlePath, onStderr, onStdout, heartbeatTimeoutMs, debug } =
      params;
    if (bundlePath === undefined) {
      throw new Error("bundle path is required");
    }

    this._debug = debug;
    this._bundlePath = bundlePath;
    this.onStdout = onStdout ?? noop;
    this.onStderr = onStderr ?? noop;
    this.heartbeatTimeoutMs = heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
  }

  get iframe() {
    return this._iframe;
  }

  static async init({ container, ...constructorParams }) {
    const popcorn = new Popcorn(constructorParams, INIT_TOKEN);
    popcorn._trace("Main: init, params: ", { container, ...constructorParams });
    await popcorn._mount(container ?? document.body);
    return popcorn;
  }

  async _mount(container) {
    if (this._iframe !== null) {
      throw new Error("Iframe already mounted");
    }

    this._trace("Main: mount, container: ", container);
    const mountPromise = new Promise((resolve) => {
      // will be resolved on init message from iframe
      this._mountPromise = resolve;

      // // example pathname: "/wasm/popcorn.js"
      // const pathName = URL.parse(import.meta.url).pathname;
      // const lastPart = /[^/]+$/;
      // const leadingSlash = /^\//;
      // const dir = pathName.replace(lastPart, "").replace(leadingSlash, "");

      // const bundleDir = "../".repeat(dir.split("/").length - 1);

      this._iframe = document.createElement("iframe");
      this._iframe.srcdoc = `<html>
        <html lang="en" dir="ltr">
            <head>
              <meta name="bundle-path" content="${"../" + this._bundlePath}" />
            </head>
            <script type="module" src="./wasm/popcorn.js" defer></script>
            <script type="module" src="./wasm/AtomVM.mjs" defer></script>
            <script type="module" src="./wasm/popcorn_iframe.js" defer></script>
            <script type="module" defer>
              import { initVm } from "./wasm/popcorn_iframe.js";
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

    this._trace("Main: deinit");
    window.removeEventListener("message", this._listenerRef);
    this._iframe.remove();
    this._iframe = null;
    this._mountPromise = null;
    this._listenerRef = null;
  }

  async call(args, { process, timeoutMs }) {
    const targetProcess = process ?? this._initProcess;
    if (this.iframe === null) {
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

  cast(args, { process }) {
    const targetProcess = process ?? this._initProcess;
    if (this.iframe === null) {
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
    } else if (data.type?.startsWith("popcorn")) {
      console.warn(
        `Received unhandled event: ${JSON.stringify(data, null, 4)}`,
      );
    }
  }

  _onInit(initProcess) {
    this._trace("Main: onInit, main process: ", initProcess);
    this._mountPromise();
    this._initProcess = initProcess;
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
      this._reloadIframe();
    }, this.heartbeatTimeoutMs);
  }

  _reloadIframe() {
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
