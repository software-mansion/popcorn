// ../../local-live-view/assets/node_modules/@swmansion/popcorn/dist/types.mjs
var INIT_VM_TIMEOUT_MS = 3e4;
var CALL_TIMEOUT_MS = 6e4;
var HEARTBEAT_TIMEOUT_MS = 6e4;
var MAX_RELOAD_N = 3;
var MESSAGES = {
  EVENT: "popcorn-event",
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
  RELOAD: "popcorn-reload"
};
var EVENT_NAMES = {
  ELIXIR_READY: "popcorn_elixir_ready",
  APP_READY: "popcorn_app_ready",
  SET_DEFAULT_RECEIVER: "popcorn_set_default_receiver"
};
var MESSAGES_TYPES = new Set(Object.values(MESSAGES));
function isMessageType(type) {
  return MESSAGES_TYPES.has(type);
}

// ../../local-live-view/assets/node_modules/@swmansion/popcorn/dist/errors.mjs
var defaultErrorMessages = {
  timeout: "Promise timeout",
  deinitialized: "Call cancelled due to instance deinit",
  reload: "Call cancelled due to iframe reload"
};
var PopcornError = class extends Error {
  code;
  constructor(code, message) {
    super(message ?? defaultErrorMessages[code]);
    this.code = code;
    this.name = "PopcornError";
  }
};
var INIT_VM_TIMEOUT_MS2 = 3e4;
var PopcornInternalError = class extends Error {
  code;
  constructor(code, message) {
    super(message ?? `Internal error: ${code}`);
    this.code = code;
    this.name = "PopcornInternalError";
  }
};
function buildError(error) {
  switch (error.t) {
    case "assert":
      return new PopcornInternalError("assert", "Assertion error");
    case "private_constructor":
      return new PopcornInternalError("private_constructor", "Don't construct the Popcorn object directly, use Popcorn.init() instead");
    case "bad_call":
      return new PopcornInternalError("bad_call", "Response for non-existent call");
    case "no_acked_call":
      return new PopcornInternalError("no_acked_call", "Response for non-acknowledged call");
    case "bad_ack":
      return new PopcornInternalError("bad_ack", "Ack for non-existent call");
    case "already_mounted":
      return new PopcornInternalError("already_mounted", "Iframe already mounted");
    case "unmounted":
      return new PopcornInternalError("unmounted", "WASM iframe not mounted");
    case "bad_target":
      return new PopcornInternalError("bad_target", "Unspecified target process");
    case "bad_status":
      return new PopcornInternalError("bad_status", `Operation not allowed: instance in "${error.status}" state, expected "${error.expectedStatus}"`);
    case "app_ready_timeout":
      return new PopcornInternalError("app_ready_timeout", `Elixir app did not call Popcorn.Wasm.ready() within ${INIT_VM_TIMEOUT_MS2}ms`);
    case "bundle_not_found":
      return new PopcornInternalError("bundle_not_found", `Could not find a valid .avm bundle at "${error.primary}" or fallback "${error.fallback}"`);
  }
}
function throwError(error) {
  throw buildError(error);
}

// ../../local-live-view/assets/node_modules/@swmansion/popcorn/dist/bridge.mjs
var STYLE_HIDDEN = "visibility: hidden; width: 0px; height: 0px; border: none";
var IframeBridge = class {
  iframe;
  handlerRef;
  // @ts-expect-error TODO: use for tracing
  debug;
  onMessage;
  constructor(args) {
    const { container, config, script, debug, onMessage } = args;
    this.debug = debug;
    this.onMessage = onMessage;
    this.iframe = document.createElement("iframe");
    this.iframe.srcdoc = `
      <html lang="en" dir="ltr">
          <head>
          ${metaTagsFrom(config)}
          </head>
          <body>
            <script type="module" defer>
              import { ${script.entrypoint} } from "${script.url}";
              ${script.entrypoint}();
            <\/script>
          </body>
      </html>`;
    this.iframe.style = STYLE_HIDDEN;
    this.handlerRef = this.messageHandler.bind(this);
    window.addEventListener("message", this.handlerRef);
    container.appendChild(this.iframe);
  }
  sendIframeRequest(data) {
    const w = this.iframe.contentWindow;
    if (w === null)
      throwError({ t: "assert" });
    w.postMessage(data);
  }
  deinit() {
    window.removeEventListener("message", this.handlerRef);
    this.iframe.remove();
  }
  messageHandler(event) {
    const fromOurIframe = event.source === this.iframe.contentWindow;
    if (!fromOurIframe || !isIframeResponse(event.data))
      return;
    this.onMessage(event.data);
  }
};
function isIframeResponse(payload) {
  if (typeof payload !== "object" || payload === null)
    return false;
  if (!Object.hasOwn(payload, "type") || !Object.hasOwn(payload, "value"))
    return false;
  if (typeof payload.type !== "string")
    return false;
  return isMessageType(payload.type);
}
function metaTagsFrom(config) {
  return Object.entries(config).map(([key, value]) => `<meta name="${key}" content="${value}" />`).join("\n");
}

// ../../local-live-view/assets/node_modules/@swmansion/popcorn/dist/popcorn.mjs
var INIT_TOKEN = /* @__PURE__ */ Symbol();
var IFRAME_URL = new URL("./iframe.mjs", import.meta.url).href;
var Popcorn = class _Popcorn {
  heartbeatTimeoutMs = null;
  onReloadCallback;
  bridge = null;
  bridgeConfig;
  debug = false;
  bundleURLs;
  state = { status: "uninitialized" };
  defaultReceiver = null;
  requestId = 0;
  calls = /* @__PURE__ */ new Map();
  logListeners = {
    stdout: /* @__PURE__ */ new Set(),
    stderr: /* @__PURE__ */ new Set()
  };
  messageHandlers = /* @__PURE__ */ new Set();
  mountResolve = null;
  heartbeatTimeout = null;
  reloadN = 0;
  constructor(params, token) {
    if (token !== INIT_TOKEN)
      throwError({ t: "private_constructor" });
    const bundlePaths = params.bundlePaths ?? ["/bundle.avm"];
    this.bundleURLs = bundlePaths.map((p) => new URL(p, import.meta.url).href);
    this.onReloadCallback = params.onReload ?? noop;
    this.debug = params.debug ?? false;
    this.bridgeConfig = {
      container: params.container,
      script: { url: IFRAME_URL, entrypoint: "initVm" },
      config: Object.fromEntries(this.bundleURLs.map((url, i) => [`bundle-path-${i}`, url])),
      debug: true,
      onMessage: this.iframeHandler.bind(this)
    };
    this.logListeners.stdout.add(params.onStdout ?? console.log);
    this.logListeners.stderr.add(params.onStderr ?? console.warn);
    this.heartbeatTimeoutMs = params.heartbeatTimeoutMs ?? HEARTBEAT_TIMEOUT_MS;
  }
  /**
   * Creates an iframe and sets up communication channels.
   * Returns after the Elixir app calls `Popcorn.Wasm.ready/0,1`.
   *
   * @example
   * import { Popcorn } from "@swmansion/popcorn";
   * const popcorn = await Popcorn.init({
   *   onStdout: console.log,
   *   onStderr: console.error,
   *   debug: true,
   * });
   */
  static async init(options) {
    const { container, ...constructorParams } = options;
    const containerWithDefault = container ?? document.documentElement;
    const bundlePaths = constructorParams.bundlePaths && constructorParams.bundlePaths.length > 0 ? constructorParams.bundlePaths : [await resolveBundleURL("/bundle.avm", "/assets/bundle.avm")];
    const popcorn = new _Popcorn({ ...constructorParams, bundlePaths, container: containerWithDefault }, INIT_TOKEN);
    popcorn.trace("Main: init, params: ", { container, ...constructorParams });
    await popcorn.mount();
    return popcorn;
  }
  async mount() {
    if (this.bridge !== null)
      throwError({ t: "already_mounted" });
    this.assertStatus(["uninitialized", "reload"]);
    this.transition({ status: "mount" });
    this.trace("Main: mount, container: ", this.bridgeConfig.container);
    this.bridge = new IframeBridge(this.bridgeConfig);
    try {
      const mountPromise = new Promise((resolve) => {
        this.mountResolve = resolve;
      });
      let initTimeout;
      await Promise.race([
        mountPromise,
        new Promise((_, reject) => {
          initTimeout = setTimeout(() => reject(buildError({ t: "app_ready_timeout" })), INIT_VM_TIMEOUT_MS);
        })
      ]);
      clearTimeout(initTimeout);
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
   * Unless passed via options, the name passed in `Popcorn.Wasm.set_default_receiver/1` on the Elixir side is used.
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
  async call(args, { process, timeoutMs } = {}) {
    this.assertStatus(["ready"]);
    const targetProcess = process ?? this.defaultReceiver;
    if (targetProcess === null)
      throwError({ t: "bad_target" });
    if (this.bridge === null)
      throwError({ t: "unmounted" });
    const requestId = this.requestId++;
    const startTimeMs = performance.now();
    const callPromise = new Promise((resolve) => {
      this.calls.set(requestId, { acknowledged: false, startTimeMs, resolve });
    });
    this.trace("Main: call: ", { requestId, process, args });
    this.bridge.sendIframeRequest({
      type: MESSAGES.CALL,
      value: { requestId, process: targetProcess, args }
    });
    const result = await withTimeout(callPromise, timeoutMs ?? CALL_TIMEOUT_MS);
    this.calls.delete(requestId);
    return result;
  }
  /**
   * Sends a message to an Elixir process (default or from options) and returns immediately.
   *
   * Unless passed via options, the name passed in `Popcorn.Wasm.set_default_receiver/1` on the Elixir side is used.
   * Throws "Unspecified target process" if default process is not set and no process is specified.
   */
  cast(args, { process } = {}) {
    this.assertStatus(["ready"]);
    const targetProcess = process ?? this.defaultReceiver;
    if (targetProcess === null)
      throwError({ t: "bad_target" });
    if (this.bridge === null)
      throwError({ t: "unmounted" });
    const requestId = this.requestId++;
    this.trace("Main: cast: ", { requestId, process, args });
    this.bridge.sendIframeRequest({
      type: MESSAGES.CAST,
      value: { requestId, process: targetProcess, args }
    });
  }
  /**
   * Destroys an iframe and resets the instance.
   */
  deinit() {
    if (this.bridge === null)
      throwError({ t: "unmounted" });
    this.trace("Main: deinit");
    this.transition({ status: "deinit" });
    this.teardownBridge("deinitialized");
    this.logListeners.stdout.clear();
    this.logListeners.stderr.clear();
    this.messageHandlers.clear();
  }
  teardownBridge(errorCode) {
    if (this.bridge) {
      this.bridge.deinit();
      this.bridge = null;
    }
    this.mountResolve = null;
    this.defaultReceiver = null;
    if (this.heartbeatTimeout) {
      clearTimeout(this.heartbeatTimeout);
      this.heartbeatTimeout = null;
    }
    for (const callData of this.calls.values()) {
      const durationMs = performance.now() - callData.startTimeMs;
      callData.resolve({
        ok: false,
        error: new PopcornError(errorCode),
        durationMs
      });
    }
    this.calls.clear();
  }
  /**
   * Registers a log listener that will be called when output of the specified type is received.
   */
  registerLogListener(listener, type) {
    this.logListeners[type].add(listener);
  }
  /**
   * Unregisters a previously registered log listener.
   */
  unregisterLogListener(listener, type) {
    this.logListeners[type].delete(listener);
  }
  notifyLogListeners(type, message) {
    this.logListeners[type].forEach((listener) => {
      listener(message);
    });
  }
  /**
   * Registers a catch-all event handler. Returns an unsubscribe function.
   */
  onMessage(handler) {
    this.messageHandlers.add(handler);
    return () => {
      this.messageHandlers.delete(handler);
    };
  }
  onEvent({ eventName, payload }) {
    if (eventName.startsWith("popcorn")) {
      if (eventName === EVENT_NAMES.ELIXIR_READY) {
        this.trace("Main: elixir VM ready");
      } else if (eventName === EVENT_NAMES.APP_READY) {
        this.defaultReceiver = payload.name;
        this.mountResolve?.();
        this.mountResolve = null;
      } else if (eventName === EVENT_NAMES.SET_DEFAULT_RECEIVER) {
        this.defaultReceiver = payload.name;
      } else {
        this.trace("Unknown internal event:", eventName);
      }
      return;
    }
    this.messageHandlers.forEach((handler) => {
      try {
        handler(eventName, payload);
      } catch (error) {
        console.error(`Error in onMessage handler for '${eventName}':`, error);
      }
    });
  }
  iframeHandler(data) {
    if (data.type === MESSAGES.EVENT) {
      this.onEvent(data.value);
    } else if (data.type === MESSAGES.STDOUT) {
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
  onCallAck({ requestId }) {
    this.assertStatus(["ready"]);
    this.trace("Main: onCallAck: ", { requestId });
    const callData = this.calls.get(requestId);
    if (callData === void 0)
      throwError({ t: "bad_ack" });
    this.calls.set(requestId, { ...callData, acknowledged: true });
  }
  onCall({ requestId, error, data }) {
    this.assertStatus(["ready"]);
    this.trace("Main: onCall: ", { requestId, error, data });
    const callData = this.calls.get(requestId);
    if (callData === void 0)
      throwError({ t: "bad_call" });
    if (!callData.acknowledged)
      throwError({ t: "no_acked_call" });
    this.calls.delete(requestId);
    const durationMs = performance.now() - callData.startTimeMs;
    if (error !== void 0) {
      callData.resolve({ ok: false, error, durationMs });
    } else {
      callData.resolve({ ok: true, data, durationMs });
    }
  }
  onHeartbeat() {
    if (this.heartbeatTimeout) {
      clearTimeout(this.heartbeatTimeout);
    }
    this.heartbeatTimeout = setTimeout(() => {
      this.trace("Main: heartbeat lost");
      this.reloadIframe("heartbeat_lost");
    }, this.heartbeatTimeoutMs);
  }
  reloadIframe(reason = "other") {
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
    this.teardownBridge("reload");
    this.onReloadCallback(reason);
    this.mount();
  }
  trace(...messages) {
    if (this.debug) {
      console.debug(...messages);
    }
  }
  transition(to) {
    this.trace(`State: ${this.state.status} -> ${to.status}`);
    this.state = to;
  }
  assertStatus(validStatuses) {
    const currentStatus = this.state.status;
    if (!validStatuses.includes(currentStatus)) {
      throwError({
        t: "bad_status",
        status: currentStatus,
        expectedStatus: validStatuses.join(" | ")
      });
    }
  }
};
async function withTimeout(promise, ms) {
  let timeout = null;
  const timeoutPromise = new Promise((resolve) => {
    timeout = setTimeout(() => {
      resolve({
        ok: false,
        error: new PopcornError("timeout"),
        durationMs: ms
      });
    }, ms);
  });
  const result = await Promise.race([promise, timeoutPromise]);
  if (!timeout)
    throwError({ t: "assert" });
  clearTimeout(timeout);
  return result;
}
function noop() {
}
async function resolveBundleURL(primary, fallback) {
  const fetchBundle = async (path) => {
    const url = new URL(path, import.meta.url).href;
    const response = await fetch(url, { method: "HEAD" });
    const contentType = response.headers.get("Content-Type") ?? "";
    if (!response.ok || contentType.includes("text/html")) {
      throw new Error(`Bundle not found at "${path}"`);
    }
    return path;
  };
  try {
    return await Promise.any([fetchBundle(primary), fetchBundle(fallback)]);
  } catch {
    throwError({ t: "bundle_not_found", primary, fallback });
  }
}

// ../../local-live-view/assets/local_live_view.js
var LLVEngine = class _LLVEngine {
  /** @param {unknown} popcorn */
  constructor(popcorn) {
    this.popcorn = popcorn;
  }
  /**
   * Initializes LLVEngine and connects the LiveSocket.
   *
   * @param {import("phoenix_live_view").LiveSocket} liveSocket
   * @param {LLVConfig} [config]
   * @returns {Promise<LLVEngine>}
   */
  static async create(liveSocket, config = {}) {
    const { Socket } = config;
    let popcorn;
    const bufferedServerMessages = [];
    window.addEventListener("phx:llv_server_message", async (e) => {
      if (!popcorn) {
        bufferedServerMessages.push(e.detail);
        return;
      }
      await sendServerMessage(popcorn, e.detail);
    });
    popcorn = await Popcorn.init({
      debug: config.debug ?? false,
      bundlePaths: config.bundlePaths ?? ["wasm/bundle.avm"]
    });
    if (config.eventHandler) {
      popcorn.onMessage(config.eventHandler);
    }
    window.__popcorn = popcorn;
    const channels = {};
    const viewsById = {};
    const mirrorEls = document.querySelectorAll(
      "[data-pop-view][data-pop-mirror]"
    );
    if (mirrorEls.length > 0) {
      const csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content");
      const llvSocket = new Socket("/llv_socket", {
        params: { _csrf_token: csrfToken }
      });
      llvSocket.connect();
      mirrorEls.forEach((el) => {
        const llvId = el.id;
        const channel = llvSocket.channel(`llv:${llvId}`, {
          view: el.dataset.popView
        });
        channels[llvId] = channel;
        channel.join().receive("ok", () => {
          if (viewsById[llvId]) {
            popcorn.call(
              { id: llvId, event: "llv_reconnected", payload: {} },
              { timeoutMs: 1e4 }
            ).catch((err) => console.error("LLV reconnect sync error", err));
          }
        }).receive(
          "error",
          (err) => console.error("LLV channel join error", err)
        );
      });
      window.__llvSync = (id, eventName, payload) => {
        const channel = channels[id];
        if (channel) {
          channel.push(eventName, payload);
        }
      };
    }
    const { data: initialRenderedByView } = await popcorn.call(
      { views: findPredefinedViews() },
      { timeoutMs: 1e4 }
    );
    const pop_view_els = Array.from(document.querySelectorAll("[data-pop-view]"));
    window.__popcornTransportReceive = function(llvId, diff) {
      const view = viewsById[llvId];
      if (!view) {
        console.error(
          "LLV view not found:",
          llvId,
          "available:",
          Object.keys(viewsById)
        );
        return;
      }
      view.update(structuredClone(diff), []);
    };
    pop_view_els.forEach((pop_view_el) => {
      const llvId = pop_view_el.id;
      const view = liveSocket.newRootView(pop_view_el);
      viewsById[llvId] = view;
      const origAddHook = view.addHook.bind(view);
      view.addHook = function(el) {
        if (el === this.el) return;
        return origAddHook(el);
      };
      view.isConnected = function() {
        return true;
      };
      view.join = function(callback) {
        this.showLoader(this.liveSocket.loaderTimeout);
        this.joinCallback = (onDone) => {
          onDone = onDone || function() {
          };
          callback ? callback(this.joinCount, onDone) : onDone();
        };
      };
      view.pushWithReply = function(refGenerator, event, payload) {
        const [ref, [_el], _opts] = refGenerator ? refGenerator({ payload }) : [null, [], {}];
        if (typeof payload.cid !== "number") {
          delete payload.cid;
        }
        popcorn.call(
          { id: this.el.id, event, payload },
          { timeoutMs: 1e4 }
        ).catch((err) => console.error("LLV view.pushWithReply error", err));
        if (ref !== null) {
          this.undoRefs(ref, payload.event || event);
        }
        return Promise.resolve({ resp: {}, reply: null, ref });
      };
      view.maybePushComponentsDestroyed = function(_destroyedCIDs) {
      };
      view.bindChannel = function() {
      };
      view.join();
      const initialRendered = initialRenderedByView[llvId];
      if (initialRendered) {
        liveSocket.requestDOMUpdate(
          () => view.onJoin({ rendered: initialRendered })
        );
      } else {
        console.error("LLV no initial rendered for view", llvId);
      }
    });
    const origOwner = liveSocket.owner.bind(liveSocket);
    liveSocket.owner = function(childEl, callback) {
      const llvEl = childEl.closest?.("[data-pop-view]");
      if (llvEl && viewsById[llvEl.id]) {
        const view = viewsById[llvEl.id];
        return callback ? callback(view) : view;
      }
      return origOwner(childEl, callback);
    };
    const buildPointerData = (e, el) => {
      const rect = el.getBoundingClientRect();
      return {
        clientX: e.clientX,
        clientY: e.clientY,
        offsetX: e.clientX - rect.left,
        offsetY: e.clientY - rect.top,
        pageX: e.pageX,
        pageY: e.pageY,
        screenX: e.screenX,
        screenY: e.screenY,
        movementX: e.movementX,
        movementY: e.movementY,
        button: e.button,
        buttons: e.buttons,
        altKey: e.altKey,
        ctrlKey: e.ctrlKey,
        metaKey: e.metaKey,
        shiftKey: e.shiftKey,
        targetWidth: el.offsetWidth ?? 0,
        targetHeight: el.offsetHeight ?? 0
      };
    };
    const closestWithBinding = (target, binding) => {
      let el = target;
      while (el && el.nodeType === 1 && !(el.getAttribute && el.getAttribute(binding))) {
        el = el.parentNode;
      }
      return el && el.nodeType === 1 && el.getAttribute(binding) ? el : null;
    };
    const mouseEventTypes = [
      "mousedown",
      "mouseup",
      "mousemove",
      "mouseover",
      "mouseout"
    ];
    for (const eventType of mouseEventTypes) {
      const elementBinding = `phx-${eventType}`;
      const windowBinding = `phx-window-${eventType}`;
      window.addEventListener(eventType, (e) => {
        const el = closestWithBinding(e.target, elementBinding);
        if (el) {
          const phxEvent = el.getAttribute(elementBinding);
          liveSocket.debounce(el, e, eventType, () => {
            liveSocket.withinOwners(el, (view) => {
              view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
            });
          });
        } else {
          document.querySelectorAll(`[${windowBinding}]`).forEach((wel) => {
            const phxEvent = wel.getAttribute(windowBinding);
            liveSocket.debounce(wel, e, eventType, () => {
              liveSocket.withinOwners(wel, (view) => {
                view.pushEvent(eventType, wel, null, phxEvent, buildPointerData(e, wel), {});
              });
            });
          });
        }
      });
    }
    const dragEventTypes = [
      "dragstart",
      "dragenter",
      "dragover",
      "dragleave",
      "drop",
      "dragend"
    ];
    const dragPreventDefault = /* @__PURE__ */ new Set(["dragenter", "dragover", "drop"]);
    for (const eventType of dragEventTypes) {
      const elementBinding = `phx-${eventType}`;
      window.addEventListener(eventType, (e) => {
        const el = closestWithBinding(e.target, elementBinding);
        if (!el) return;
        if (dragPreventDefault.has(eventType)) {
          e.preventDefault();
        }
        if (e.dataTransfer) {
          if (eventType === "dragstart") {
            e.dataTransfer.effectAllowed = "move";
            try {
              e.dataTransfer.setData("text/plain", el.id || "");
            } catch (_) {
            }
          } else if (eventType === "dragover") {
            e.dataTransfer.dropEffect = "move";
          }
        }
        const phxEvent = el.getAttribute(elementBinding);
        liveSocket.debounce(el, e, eventType, () => {
          liveSocket.withinOwners(el, (view) => {
            view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
          });
        });
      });
    }
    for (const detail of bufferedServerMessages) {
      await sendServerMessage(popcorn, detail);
    }
    return new _LLVEngine(popcorn);
  }
  /**
   * Pushes an event into a LLVEngine from external JavaScript.
   *
   * @param {string} viewId - The view name (e.g. `"ThermostatLive"`) or element id.
   * @param {string} event - The event name to dispatch into the view's `handle_info/2`.
   * @param {object} [payload] - Optional payload map passed alongside the event.
   * @returns {Promise<void>}
   */
  async pushEvent(viewId, event, payload = {}) {
    const el = document.querySelector(`[data-pop-view="${viewId}"]`);
    const llvId = el ? el.id : viewId;
    const result = await this.popcorn.call(
      { id: llvId, event: "llv_push", payload: { event, payload } },
      { timeoutMs: 1e4 }
    );
    if (!result.ok) {
      console.error(
        `LLV pushEvent error for view "${viewId}", event "${event}":`,
        result
      );
    }
  }
};
async function sendServerMessage(popcorn, detail) {
  const el = document.querySelector(`[data-pop-view="${detail.view}"]`);
  const llvId = el ? el.id : detail.view;
  await popcorn.call(
    {
      id: llvId,
      event: "llv_server_message",
      payload: {
        event: "llv_server_message",
        value: detail.payload,
        type: "llv_server_message"
      }
    },
    { timeoutMs: 1e4 }
  );
}
function findPredefinedViews() {
  return Array.from(document.querySelectorAll("[data-pop-view]")).map((el) => ({
    view: el.getAttribute("data-pop-view"),
    id: el.id
  }));
}
export {
  LLVEngine
};
