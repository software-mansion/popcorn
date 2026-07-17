const INIT_VM_TIMEOUT_MS$1 = 30_000;
const CALL_TIMEOUT_MS = 60_000;
const HEARTBEAT_TIMEOUT_MS = 60_000;
const MAX_RELOAD_N = 3;
const MESSAGES = {
    EVENT: "popcorn-event",
    CALL: "popcorn-call",
    CAST: "popcorn-cast",
    CALL_ACK: "popcorn-callAck",
    STDOUT: "popcorn-stdout",
    STDERR: "popcorn-stderr",
    HEARTBEAT: "popcorn-heartbeat",
    RELOAD: "popcorn-reload",
};
const EVENT_NAMES = {
    ELIXIR_READY: "popcorn_elixir_ready",
    APP_READY: "popcorn_app_ready",
    SET_DEFAULT_RECEIVER: "popcorn_set_default_receiver",
};
const MESSAGES_TYPES = new Set(Object.values(MESSAGES));
function isMessageType(type) {
    return MESSAGES_TYPES.has(type);
}

const defaultErrorMessages = {
    timeout: "Promise timeout",
    deinitialized: "Call cancelled due to instance deinit",
    reload: "Call cancelled due to iframe reload",
};
/** Recoverable error returned in CallResult (never thrown) */
class PopcornError extends Error {
    code;
    constructor(code, message) {
        super(message ?? defaultErrorMessages[code]);
        this.code = code;
        this.name = "PopcornError";
    }
}
const INIT_VM_TIMEOUT_MS = 30_000;
/** Non-recoverable error indicating a bug or library misuse (always thrown) */
class PopcornInternalError extends Error {
    code;
    constructor(code, message) {
        super(message ?? `Internal error: ${code}`);
        this.code = code;
        this.name = "PopcornInternalError";
    }
}
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
            return new PopcornInternalError("app_ready_timeout", `Elixir app did not call Popcorn.Wasm.ready() within ${INIT_VM_TIMEOUT_MS}ms`);
        case "bundle_not_found":
            return new PopcornInternalError("bundle_not_found", `Could not find a valid .avm bundle at "${error.primary}" or fallback "${error.fallback}"`);
    }
}
function throwError(error) {
    throw buildError(error);
}

const STYLE_HIDDEN = "visibility: hidden; width: 0px; height: 0px; border: none";
class IframeBridge {
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
            </script>
          </body>
      </html>`;
        this.iframe.style = STYLE_HIDDEN;
        this.handlerRef = this.messageHandler.bind(this);
        window.addEventListener("message", this.handlerRef);
        // mount
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
}
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
    return Object.entries(config)
        .map(([key, value]) => `<meta name="${key}" content="${value}" />`)
        .join("\n");
}

const INIT_TOKEN = Symbol();
const IFRAME_URL = new URL("./iframe.mjs", import.meta.url).href;
/**
 * Manages Elixir by setting up iframe, WASM module, and event listeners. Used to sent messages to Elixir processes.
 */
class Popcorn {
    heartbeatTimeoutMs = null;
    onReloadCallback;
    bridge = null;
    bridgeConfig;
    debug = false;
    bundleURLs;
    state = { status: "uninitialized" };
    defaultReceiver = null;
    requestId = 0;
    calls = new Map();
    logListeners = {
        stdout: new Set(),
        stderr: new Set(),
    };
    messageHandlers = new Set();
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
            onMessage: this.iframeHandler.bind(this),
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
        const bundlePaths = constructorParams.bundlePaths && constructorParams.bundlePaths.length > 0
            ? constructorParams.bundlePaths
            : [await resolveBundleURL("/bundle.avm", "/assets/bundle.avm")];
        const popcorn = new Popcorn({ ...constructorParams, bundlePaths, container: containerWithDefault }, INIT_TOKEN);
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
                    initTimeout = setTimeout(() => reject(buildError({ t: "app_ready_timeout" })), INIT_VM_TIMEOUT_MS$1);
                }),
            ]);
            clearTimeout(initTimeout);
            this.transition({ status: "ready" });
            this.trace("Main: mounted");
            this.onHeartbeat();
        }
        catch (error) {
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
            value: { requestId, process: targetProcess, args },
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
            value: { requestId, process: targetProcess, args },
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
                durationMs,
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
            }
            else if (eventName === EVENT_NAMES.APP_READY) {
                this.defaultReceiver = payload.name;
                this.mountResolve?.();
                this.mountResolve = null;
            }
            else if (eventName === EVENT_NAMES.SET_DEFAULT_RECEIVER) {
                this.defaultReceiver = payload.name;
            }
            else {
                this.trace("Unknown internal event:", eventName);
            }
            return;
        }
        this.messageHandlers.forEach((handler) => {
            try {
                handler(eventName, payload);
            }
            catch (error) {
                console.error(`Error in onMessage handler for '${eventName}':`, error);
            }
        });
    }
    iframeHandler(data) {
        if (data.type === MESSAGES.EVENT) {
            this.onEvent(data.value);
        }
        else if (data.type === MESSAGES.STDOUT) {
            this.notifyLogListeners("stdout", data.value);
        }
        else if (data.type === MESSAGES.STDERR) {
            this.notifyLogListeners("stderr", data.value);
        }
        else if (data.type === MESSAGES.CALL) {
            this.onCall(data.value);
        }
        else if (data.type === MESSAGES.CALL_ACK) {
            this.onCallAck(data.value);
        }
        else if (data.type === MESSAGES.HEARTBEAT) {
            this.onHeartbeat();
        }
        else if (data.type === MESSAGES.RELOAD) {
            this.reloadIframe();
        }
        else {
            throwError({ t: "assert" });
        }
    }
    onCallAck({ requestId }) {
        this.assertStatus(["ready"]);
        this.trace("Main: onCallAck: ", { requestId });
        const callData = this.calls.get(requestId);
        if (callData === undefined)
            throwError({ t: "bad_ack" });
        this.calls.set(requestId, { ...callData, acknowledged: true });
    }
    onCall({ requestId, error, data, }) {
        this.assertStatus(["ready"]);
        this.trace("Main: onCall: ", { requestId, error, data });
        const callData = this.calls.get(requestId);
        if (callData === undefined)
            throwError({ t: "bad_call" });
        if (!callData.acknowledged)
            throwError({ t: "no_acked_call" });
        this.calls.delete(requestId);
        const durationMs = performance.now() - callData.startTimeMs;
        if (error !== undefined) {
            callData.resolve({ ok: false, error, durationMs });
        }
        else {
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
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
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
                expectedStatus: validStatuses.join(" | "),
            });
        }
    }
}
async function withTimeout(promise, ms) {
    let timeout = null;
    const timeoutPromise = new Promise((resolve) => {
        timeout = setTimeout(() => {
            resolve({
                ok: false,
                error: new PopcornError("timeout"),
                durationMs: ms,
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
    /* noop */
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
    }
    catch {
        throwError({ t: "bundle_not_found", primary, fallback });
    }
}

// Wire a fake Phoenix root view around a [data-pop-view] element so the
// browser renders the runtime's diffs and routes events to it.
function setupFakeView(socket, views, popcornSocket, pop_view_el) {
    const llvId = pop_view_el.id;
    const view = socket.newRootView(pop_view_el);
    views.set(llvId, view);
    // The view's channel: a real Phoenix Channel on the popcorn socket.
    // Everything channel-shaped — join handshake, event acks carrying
    // diffs, out-of-band "diff" frames, ref bookkeeping — runs through the
    // stock Channel/Push machinery over the PopcornTransport.
    view.channel = popcornSocket.channel(`lv:${llvId}`);
    // addHook: skip the root element to prevent Phoenix from trying to register it
    // as a hook within this view's scope — hooks on children are still processed normally.
    const origAddHook = view.addHook.bind(view);
    view.addHook = function (el) {
        if (el === this.el)
            return;
        return origAddHook(el);
    };
    // Stock join: bindChannel + channel.join over the popcorn transport.
    // The join frame is answered by the view's process itself, serving the
    // rendered it produced at mount — so onJoin runs the regular path.
    // No need to set data-phx-session / data-phx-root-id: owner() is
    // overridden by the engine to route events via [data-pop-view] lookup
    // instead of attribute-based lookup.
    view.join();
}

const llvIdFromTopic = (topic) => topic.slice("lv:".length);
// Frames the WASM view must answer. Each entry needs a matching
// Message clause in LocalLiveView.Server.
// Anything else — heartbeats, phx_leave — is acked in place
// as Popcorn may be not booted yet.
const ANSWERED_EVENTS = ["phx_join", "event", "cids_will_destroy", "cids_destroyed"];
// A fake socket that connects LLV vievs to Popcorn.
// Phoenix channels can be normally constructed on top of this socket.
function createPopcornSocket(SocketClass, pop) {
    let transport = null;
    class PopcornTransport {
        readyState = 0; // CONNECTING
        onopen = () => { };
        onerror = () => { };
        onmessage = () => { };
        onclose = () => { };
        // The WebSocket-shaped signature the Socket constructs us with; the URL
        // is a dead label.
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        constructor(_endpointURL, _protocols) {
            // eslint-disable-next-line @typescript-eslint/no-this-alias
            transport = this;
            // The Socket assigns onopen/onmessage/onclose after `new`, so the
            // "connection" must open asynchronously.
            queueMicrotask(() => {
                this.readyState = 1; // OPEN
                this.onopen();
            });
        }
        // Deliver an inbound frame. Async so an ack never re-enters Socket code
        // in the middle of an outbound send.
        inject(frame) {
            queueMicrotask(() => {
                if (this.readyState === 1)
                    this.onmessage({ data: frame });
            });
        }
        // Ack an outbound frame in place (joins, leaves, heartbeats, no-ops).
        ack(frame, status, response) {
            this.inject({
                topic: frame.topic,
                event: "phx_reply",
                payload: { status, response },
                ref: frame.ref,
                join_ref: frame.join_ref,
            });
        }
        send(frame) {
            const { topic, event, payload } = frame;
            if (!ANSWERED_EVENTS.includes(event)) {
                this.ack(frame, "ok", {});
                return;
            }
            pop.handleTransportFrame(llvIdFromTopic(topic), event, payload).then((result) => {
                if (result.ok) {
                    const { status, payload: response } = result.data;
                    this.ack(frame, status, response);
                }
                else {
                    this.ack(frame, "error", result.error);
                }
            }, (err) => this.ack(frame, "error", String(err)));
        }
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
        close(_code, _reason) {
            this.readyState = 3; // CLOSED
            queueMicrotask(() => this.onclose({ code: 1000, wasClean: true }));
        }
    }
    // The endpoint URL is only a label — the transport never dereferences it.
    const socket = new SocketClass("/llv-popcorn", {
        transport: PopcornTransport,
        encode: (payload, callback) => callback(payload),
        decode: (rawPayload, callback) => callback(rawPayload),
    });
    socket.connect();
    return {
        socket,
        inject(frame) {
            transport?.inject(frame);
        },
    };
}

// LLV navigation runs in one of two modes:
//
//  - Hosted: the page has a connected host LiveView (liveSocket.main). Phoenix owns
//    the browser history and the popstate handler, so we route LLV navigation through
//    Phoenix (pushHistoryPatch) and just listen to phx:navigate to re-run handle_params
//    in the LLV views. This keeps the host LV and history in sync
//
//  - Standalone: the page is rendered with no host LiveView.
//    There is no Phoenix popstate or [data-phx-link] click handler.
//    LLV must own navigation itself: intercept patch-link clicks, push the
//    history entry, and handle popstate.
function registerNavigationHandlers(socket, views, pop, config) {
    const absHref = (href) => new URL(href, window.location.origin).href;
    const phoenixOwnsNav = () => socket.isConnected();
    const llvHandleParams = (href) => {
        const url = absHref(href);
        const params = Object.fromEntries(new URL(url).searchParams.entries());
        for (const llvId of views.keys()) {
            pop.handleParams(llvId, params, url);
        }
    };
    let lastLLVNavigatedHref = null;
    // Standalone-only: intercept clicks on patch links. Lets one `<.link patch>` work in
    // both modes, no separate LLV link component needed.
    document.addEventListener("click", (e) => {
        if (phoenixOwnsNav())
            return;
        const link = e.target.closest('a[data-phx-link="patch"]');
        if (!link)
            return;
        e.preventDefault();
        const to = link.getAttribute("href") ?? window.location.href;
        const replace = link.getAttribute("data-phx-link-state") === "replace";
        if (replace) {
            window.history.replaceState({ llv: true }, "", to);
        }
        else {
            window.history.pushState({ llv: true }, "", to);
        }
        llvHandleParams(to);
    });
    window.addEventListener("popstate", () => {
        if (phoenixOwnsNav())
            return;
        llvHandleParams(window.location.href);
    });
    // llv:navigate: LLV push_patch fires this event after the WASM-side
    // handle_params has run. We write the history entry per mode:
    //  - hosted: hand to Phoenix via pushHistoryPatch (Phoenix-owned patch entry + host
    //    handle_params); the phx:navigate echo is skipped via lastLLVNavigatedHref.
    //  - standalone: just write the URL bar — the WASM side already ran handle_params.
    window.addEventListener("llv:navigate", (e) => {
        const { href, replace } = e.detail;
        lastLLVNavigatedHref = absHref(href);
        if (config.onNavigate) {
            config.onNavigate(href, replace);
            return;
        }
        if (phoenixOwnsNav()) {
            socket.pushHistoryPatch({ isTrusted: false, type: "llv:navigate" }, href, replace ? "replace" : "push", null);
        }
        else if (replace) {
            window.history.replaceState({ llv: true }, "", href);
        }
        else {
            window.history.pushState({ llv: true }, "", href);
        }
    });
    // phx:navigate: forward Phoenix LiveView patch navigations to all LLV views (hosted
    // mode only — never dispatched in dead mode). Fires for <.link patch> clicks and
    // browser back/forward. Skip navigations LLV itself triggered via push_patch, since
    // LLV already ran handle_params on the WASM side for those.
    window.addEventListener("phx:navigate", (e) => {
        const detail = e.detail;
        if (!detail?.patch)
            return;
        const url = absHref(detail.href ?? window.location.href);
        if (url === lastLLVNavigatedHref) {
            lastLLVNavigatedHref = null;
            return;
        }
        llvHandleParams(url);
    });
}

// Phoenix LiveView only binds click/keydown/keyup/blur/focus natively, so
// phx-mouse*/phx-window-mouse* and the HTML5 phx-drag* bindings are no-ops.
// Wire them up here, walking up from e.target to find the closest ancestor with
// the binding (same as how phx-click works) so element-level bindings on
// container elements also catch events from their descendants.
function registerCustomEventBindings(socket) {
    // The binding element's bounding rect is included so handlers can
    // position-aware-route the event (e.g. clientY - rect.top for the offset
    // within the element, rect.height for its size).
    const buildPointerData = (e, el) => {
        const rect = el.getBoundingClientRect();
        return {
            clientX: e.clientX,
            clientY: e.clientY,
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
            rect: {
                top: rect.top,
                left: rect.left,
                width: rect.width,
                height: rect.height,
            },
        };
    };
    const isElement = (node) => node?.nodeType === Node.ELEMENT_NODE;
    // Walk up from `target` to the nearest element that carries the `binding`
    // attribute (e.g. "phx-dragover"), returning it or null if none is found.
    // Mirrors how phx-click resolves a handler from the event's deepest target.
    const closestWithBinding = (target, binding) => {
        let el = target;
        while (el && isElement(el) && !el.getAttribute(binding)) {
            el = el.parentNode;
        }
        return isElement(el) && el.getAttribute(binding) ? el : null;
    };
    const mouseEventTypes = ["mousedown", "mouseup", "mousemove", "mouseover", "mouseout"];
    for (const eventType of mouseEventTypes) {
        const elementBinding = `phx-${eventType}`;
        const windowBinding = `phx-window-${eventType}`;
        window.addEventListener(eventType, (e) => {
            const el = closestWithBinding(e.target, elementBinding);
            if (el) {
                const phxEvent = el.getAttribute(elementBinding);
                socket.debounce(el, e, eventType, () => {
                    socket.withinOwners(el, (view) => {
                        view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
                    });
                });
            }
            else {
                document.querySelectorAll(`[${windowBinding}]`).forEach((wel) => {
                    const phxEvent = wel.getAttribute(windowBinding);
                    socket.debounce(wel, e, eventType, () => {
                        socket.withinOwners(wel, (view) => {
                            view.pushEvent(eventType, wel, null, phxEvent, buildPointerData(e, wel), {});
                        });
                    });
                });
            }
        });
    }
    // HTML5 drag-and-drop. The browser only allows a drop — and shows the move
    // cursor — when dragenter/dragover call preventDefault() on a drop target,
    // and drop must preventDefault() too. dragstart seeds dataTransfer so Firefox
    // actually initiates the drag. Bindings are read off phx-drag* attributes and
    // dispatched just like the mouse events above, with the same pointer data so
    // handlers can use the rect to decide drop position.
    const dragEventTypes = [
        "dragstart",
        "dragenter",
        "dragover",
        "dragleave",
        "drop",
        "dragend",
    ];
    const dragPreventDefault = new Set(["dragenter", "dragover", "drop"]);
    for (const eventType of dragEventTypes) {
        const elementBinding = `phx-${eventType}`;
        window.addEventListener(eventType, (e) => {
            const el = closestWithBinding(e.target, elementBinding);
            if (!el)
                return;
            // preventDefault must run on every event (not just the throttled ones)
            // or the browser keeps rejecting the drop mid-drag.
            if (dragPreventDefault.has(eventType)) {
                e.preventDefault();
            }
            if (e.dataTransfer) {
                if (eventType === "dragstart") {
                    e.dataTransfer.effectAllowed = "move";
                    try {
                        e.dataTransfer.setData("text/plain", el.id || "");
                    }
                    catch {
                        // setData throws in some browsers outside a trusted dragstart context
                    }
                }
                else if (eventType === "dragover") {
                    e.dataTransfer.dropEffect = "move";
                }
            }
            const phxEvent = el.getAttribute(elementBinding);
            socket.debounce(el, e, eventType, () => {
                socket.withinOwners(el, (view) => {
                    view.pushEvent(eventType, el, null, phxEvent, buildPointerData(e, el), {});
                });
            });
        });
    }
}

// Resolve an LLV's element id from a view name or id: prefer the element whose
// data-pop-view matches, else fall back to treating the argument as an id.
function resolveLlvId(viewOrId) {
    const el = Array.from(document.querySelectorAll("[data-pop-view]")).find((e) => e.getAttribute("data-pop-view") === viewOrId);
    return el ? el.id : viewOrId;
}
// handles messages sent by the server via LocalLiveView.push_server_message
function sendServerMessage(pop, detail) {
    pop.serverMessage(resolveLlvId(detail.view), {
        event: "llv_server_message",
        value: detail.payload,
        type: "llv_server_message",
    });
}
// data-pop-assigns holds the JSON a local_component serialized from its inline
// assigns. Absent/empty for plain local views — default to {} so the process is
// always handed a map.
function parseAssigns(raw) {
    if (!raw)
        return {};
    try {
        return JSON.parse(raw);
    }
    catch (err) {
        console.error("LLV failed to parse data-pop-assigns:", raw, err);
        return {};
    }
}

const DEFAULT_CALL_TIMEOUT_MS = 10_000;
class PopcornClient {
    popcorn = null;
    get ready() {
        return this.popcorn !== null;
    }
    attach(popcorn) {
        this.popcorn = popcorn;
    }
    call(args) {
        if (!this.popcorn) {
            return Promise.reject(new Error("LLV: PopcornClient used before runtime was ready"));
        }
        return this.popcorn.call(args, { timeoutMs: DEFAULT_CALL_TIMEOUT_MS });
    }
    fire(action, args) {
        this.call(args).then((result) => {
            if (!result.ok)
                console.error(`LLV ${action} error`, result.error);
        }, (err) => console.error(`LLV ${action} error`, err));
    }
    create({ id, view, url, urlParams, assigns }) {
        return this.call({
            action: "create",
            id,
            view,
            url,
            url_params: urlParams,
            assigns,
        });
    }
    destroy(id) {
        this.fire("destroy", { action: "destroy", id, payload: {} });
    }
    reconnected(id) {
        this.fire("reconnect sync", { action: "reconnected", id, payload: {} });
    }
    updateAssigns(id, assigns) {
        this.fire("update assigns", { action: "update_assigns", id, assigns });
    }
    handleParams(id, params, url) {
        this.fire("handle_params", { action: "handle_params", id, payload: { params, url } });
    }
    // A channel frame the view must answer: the view's process settles the
    // call promise (Popcorn.Wasm.resolve) once the frame is processed, so the
    // result carries the channel ack. The transport consumes it — no `fire`.
    handleTransportFrame(id, event, payload) {
        return this.call({ action: "transport_frame", id, event, payload });
    }
    serverMessage(id, payload) {
        this.fire("server_message", { action: "server_message", id, payload });
    }
    // Report a failed local→server push back to the view's process so its
    // handle_push_error callback runs. `payload` is the map the view passed
    // to push_server_event.
    pushError(id, event, payload) {
        this.fire("push_error report", { action: "push_error", id, payload: { event, payload } });
    }
    push(id, event, payload) {
        return this.call({ action: "push", id, payload: { event, payload } });
    }
}
class LLVEngine {
    socket;
    config;
    pop = new PopcornClient();
    views = new Map();
    channels = {};
    bufferedServerMessages = [];
    // llvId -> mounted LocalLiveViewEventBus hook: the host-side channel used
    // by __llvPushServer.
    eventBusHooks = new Map();
    popcornLink;
    constructor(socket, config) {
        this.socket = socket;
        this.config = config;
    }
    /**
     * Initializes LLVEngine and connects the LiveSocket.
     *
     * @param liveSocket - The phoenix_live_view LiveSocket instance.
     * @param config - Optional LLV configuration.
     */
    static async create(liveSocket, config = {}) {
        const engine = new LLVEngine(liveSocket, config);
        // Synchronous registration must happen BEFORE any awaits (see
        // registerServerMessageListener / registerHooks comments).
        engine.registerServerMessageListener();
        registerNavigationHandlers(engine.socket, engine.views, engine.pop, engine.config);
        engine.registerHooks();
        engine.bindFormsIfHostless();
        engine.connectPopcornSocket();
        await engine.bootPopcorn();
        engine.setupMirrorChannels();
        engine.exposeGlobals();
        engine.patchOwner();
        registerCustomEventBindings(engine.socket);
        await engine.scanAndMount();
        engine.flushBufferedServerMessages();
        return engine;
    }
    // The app's Phoenix Socket class, recovered from the live instance the
    // LiveSocket already holds — same class, same version, same module as the
    // one LiveView runs on, with nothing to configure.
    socketClass() {
        return this.socket.getSocket().constructor;
    }
    connectPopcornSocket() {
        this.popcornLink = createPopcornSocket(this.socketClass(), this.pop);
    }
    // Start a view and wire it up.
    async mountView(pop_view_el) {
        const llvId = pop_view_el.id;
        if (this.views.has(llvId))
            return;
        const result = await this.pop.create({
            id: llvId,
            view: pop_view_el.getAttribute("data-pop-view"),
            url: window.location.href,
            urlParams: Object.fromEntries(new URLSearchParams(window.location.search)),
            assigns: parseAssigns(pop_view_el.getAttribute("data-pop-assigns")),
        });
        // A rejected call resolves with ok: false (it does not throw). Bail on
        // failed or duplicate creates — wiring a fake view without a live
        // process would join a channel nobody answers.
        if (!result.ok) {
            console.error("LLV failed to create view", llvId, result.error);
            return;
        }
        const liveEl = document.getElementById(llvId);
        if (liveEl?.matches("[data-pop-view]")) {
            setupFakeView(this.socket, this.views, this.popcornLink.socket, liveEl);
        }
        else {
            this.pop.destroy(llvId);
        }
    }
    // Stop a view's runtime process and drop its fake view. Used by the hook when
    // the host LiveView removes the mount point.
    unmountView(pop_view_el) {
        const llvId = pop_view_el.id;
        const view = this.views.get(llvId);
        if (!view)
            return;
        this.views.delete(llvId);
        view.destroy?.();
        this.pop.destroy(llvId);
    }
    // Register the server message listener immediately — BEFORE any awaits.
    // push_event("llv_server_message") from Phoenix LiveView fires during the initial
    // LiveView join, which happens before Popcorn finishes initializing. Without this,
    // the event is dispatched on window before our listener is registered and is lost.
    registerServerMessageListener() {
        window.addEventListener("phx:llv_server_message", (e) => {
            const detail = e.detail;
            if (!this.pop.ready) {
                this.bufferedServerMessages.push(detail);
                return;
            }
            sendServerMessage(this.pop, detail);
        });
    }
    // Hooks that manage views rendered inside a host LiveView (other views are
    // mounted by the startup scan, which also catches hooks that fired before
    // Popcorn was ready) and the per-view event bus.
    registerHooks() {
        const pop = this.pop;
        const mountView = (el) => this.mountView(el);
        const unmountView = (el) => this.unmountView(el);
        this.socket.hooks.LocalLiveView = {
            mounted() {
                this.llvLastAssigns = this.el.getAttribute("data-pop-assigns");
                if (pop.ready)
                    mountView(this.el);
            },
            updated() {
                const raw = this.el.getAttribute("data-pop-assigns");
                if (raw === this.llvLastAssigns)
                    return;
                this.llvLastAssigns = raw;
                // Not mounted yet (Popcorn still booting): the mount reads the current
                // assigns, so there's nothing to forward. Once mounted, the dispatcher
                // processes this after the mount (it's sent after, and calls are FIFO).
                if (!pop.ready)
                    return;
                pop.updateAssigns(this.el.id, parseAssigns(raw));
            },
            destroyed() {
                unmountView(this.el);
            },
        };
        // Hook for sending events from client to server via `LocalLiveView.push_server_event`
        // Sending an event via a hook freezes the element the hook binds to along with all
        // its descendants until the event is internally ACKed by LiveView, thus we use
        // a dedicated, empty div for that.
        const eventBusHooks = this.eventBusHooks;
        this.socket.hooks.LocalLiveViewEventBus = {
            mounted() {
                const llvId = this.el.getAttribute("data-llv-event-bus-for");
                if (llvId)
                    eventBusHooks.set(llvId, this);
            },
            destroyed() {
                const llvId = this.el.getAttribute("data-llv-event-bus-for");
                if (llvId && eventBusHooks.get(llvId) === this) {
                    eventBusHooks.delete(llvId);
                }
            },
        };
    }
    // Pages with only LocalLiveViews (no server-side LiveView) connect in "dead"
    // mode, which skips bindForms() — making phx-submit / phx-change no-ops on
    // any LLV. Wire them up manually when no real LiveView is on the page.
    bindFormsIfHostless() {
        if (!document.querySelector("[data-phx-session]")) {
            this.socket.bindForms();
        }
    }
    async bootPopcorn() {
        const popcorn = await Popcorn.init({
            debug: this.config.debug ?? false,
            bundlePaths: this.config.bundlePaths ?? ["wasm/bundle.avm"],
        });
        this.pop.attach(popcorn);
        if (this.config.eventHandler) {
            popcorn.onMessage(this.config.eventHandler);
        }
    }
    // Mirror channels: only created for views with a server-side Mirror module.
    setupMirrorChannels() {
        const mirrorEls = document.querySelectorAll("[data-pop-view][data-pop-mirror-token]");
        if (mirrorEls.length === 0)
            return;
        const Socket = this.socketClass();
        const csrfToken = document.querySelector("meta[name='csrf-token']")?.getAttribute("content");
        const llvSocket = new Socket("/llv_socket", {
            params: { _csrf_token: csrfToken },
        });
        llvSocket.connect();
        mirrorEls.forEach((el) => {
            const llvId = el.id;
            const channel = llvSocket.channel(`llv:${llvId}`, {
                view: el.dataset.popView,
                token: el.dataset.popMirrorToken,
            });
            this.channels[llvId] = channel;
            channel
                .join()
                .receive("ok", () => {
                if (this.views.has(llvId)) {
                    this.pop.reconnected(llvId);
                }
            })
                .receive("error", (err) => console.error("LLV channel join error", err));
        });
        window.__llvSync = (id, eventName, payload) => {
            const channel = this.channels[id];
            if (channel) {
                channel.push(eventName, payload);
            }
        };
    }
    exposeGlobals() {
        // Out-of-band diffs (handle_info renders, send_update, push_error
        // rollbacks, server messages — anything not acking a push). Injected as
        // a channel "diff" frame, handled by the stock bindChannel handler
        // exactly like a server-pushed diff. A diff racing a just-unmounted
        // view is dropped.
        window.__popcornTransportReceive = (llvId, diff) => {
            if (!this.views.has(llvId))
                return;
            this.popcornLink.inject({
                topic: `lv:${llvId}`,
                event: "diff",
                payload: diff,
                ref: null,
                join_ref: null,
            });
        };
        // __llvPushServer: sends an event to the host LiveView, called from a
        // local view's Elixir via LocalLiveView.push_server_event/3. Pushes
        // through the event bus hook's documented, promise-returning pushEvent —
        // it rejects when the host is disconnected, replies with an error, or
        // times out; no event bus means no host LiveView on the page. Failures
        // are reported back so the view's handle_push_error runs.
        window.__llvPushServer = (llvId, event, payload) => {
            const eventBus = this.eventBusHooks.get(llvId);
            if (!eventBus) {
                console.error("LLV push_server_event: no host event bus for view", llvId);
                this.pop.pushError(llvId, event, payload);
                return;
            }
            eventBus.pushEvent(event, payload).catch((err) => {
                console.error("LLV push_server_event failed", err);
                this.pop.pushError(llvId, event, payload);
            });
        };
    }
    // owner: route events from inside [data-pop-view] elements to our fake views.
    // We never set data-phx-session on LLV elements, so Phoenix's default closestViewEl()
    // would walk up to the parent LiveView and dispatch events there instead.
    patchOwner() {
        const views = this.views;
        const origOwner = this.socket.owner.bind(this.socket);
        this.socket.owner = function (childEl, callback) {
            const llvEl = childEl.closest("[data-pop-view]");
            const view = llvEl ? views.get(llvEl.id) : undefined;
            if (view) {
                return callback ? callback(view) : view;
            }
            return origOwner(childEl, callback);
        };
    }
    // Startup scan: mount every [data-pop-view] present now that Popcorn is up.
    // This is the mount path for host-less pages (no hooks fire there) and the
    // catch-up for hooks that fired before Popcorn was ready.
    // If a view is mounted twice (here and by the hook), the dispatcher
    // on the Elixir side rejects the second create.
    async scanAndMount() {
        const pop_view_els = Array.from(document.querySelectorAll("[data-pop-view]"));
        await Promise.all(pop_view_els.map((el) => this.mountView(el)));
    }
    // Flush any server messages that arrived during Popcorn initialization.
    flushBufferedServerMessages() {
        for (const detail of this.bufferedServerMessages) {
            sendServerMessage(this.pop, detail);
        }
        this.bufferedServerMessages = [];
    }
    /**
     * Pushes an event into a LLVEngine from external JavaScript.
     *
     * @param viewId - The view name (e.g. `"ThermostatLive"`) or element id.
     * @param event - The event name to dispatch into the view's `handle_info/2`.
     * @param payload - Optional payload map passed alongside the event.
     */
    async pushEvent(viewId, event, payload = {}) {
        const result = await this.pop.push(resolveLlvId(viewId), event, payload);
        if (!result.ok) {
            console.error(`LLV pushEvent error for view "${viewId}", event "${event}":`, result);
        }
    }
}

export { LLVEngine };
