import init from './AtomVM.mjs';

const HEARTBEAT_INTERVAL_MS = 500;
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
new Set(Object.values(MESSAGES));

function sendIframeResponse(type, data) {
    window.parent.postMessage({ type, value: data });
}

// @ts-expect-error atomvm doesn't have types yet
let Module = null;
class TrackedValue {
    key;
    value;
    constructor({ key, value }) {
        if (typeof key !== "number") {
            throw new Error("key property in TrackedValue must be a number");
        }
        this.key = key;
        this.value = value;
    }
}
globalThis.TrackedValue = TrackedValue;
async function initVm() {
    const bundlePaths = Array.from(document.querySelectorAll('meta[name^="bundle-path-"]'), (el) => el.content);
    const bundles = await Promise.all(bundlePaths.map(async (p) => {
        const resp = await fetch(p);
        const buf = await resp.arrayBuffer();
        return new Int8Array(buf);
    }));
    await startVm(bundles);
    window.addEventListener("message", async ({ data }) => {
        const type = data.type;
        if (type === MESSAGES.CALL) {
            await handleCall(data.value);
        }
        else if (type === MESSAGES.CAST) {
            handleCast(data.value);
        }
    });
    setInterval(() => sendIframeResponse(MESSAGES.HEARTBEAT, null), HEARTBEAT_INTERVAL_MS);
}
async function startVm(avmBundles) {
    const bundleFilePaths = avmBundles.map((_, i) => `/data/bundle-${i}.avm`);
    const moduleInstance = await init({
        preRun: [
            function ({ FS }) {
                FS.mkdir("/data");
                avmBundles.forEach((bundle, i) => {
                    FS.writeFile(bundleFilePaths[i], bundle);
                });
            },
        ],
        arguments: bundleFilePaths,
        print(text) {
            sendIframeResponse(MESSAGES.STDOUT, text);
        },
        printErr(text) {
            sendIframeResponse(MESSAGES.STDERR, text);
        },
        onAbort() {
            // Timeout so that error logs are (hopefully) printed
            // before we terminate
            setTimeout(() => sendIframeResponse(MESSAGES.RELOAD, null), 100);
        },
    });
    Module = moduleInstance;
    moduleInstance["serialize"] = JSON.stringify;
    moduleInstance["deserialize"] = deserialize;
    moduleInstance["cleanupFunctions"] = new Map();
    moduleInstance["onTrackedObjectDelete"] = (key) => {
        const fns = moduleInstance["cleanupFunctions"];
        const fn = fns.get(key);
        fns.delete(key);
        try {
            fn?.();
        }
        catch (e) {
            console.error(e);
        }
        finally {
            moduleInstance["trackedObjectsMap"].delete(key);
        }
    };
    const origCast = moduleInstance["cast"];
    const origCall = moduleInstance["call"];
    moduleInstance["cast"] = (process, args) => {
        const serialized = moduleInstance.serialize(args);
        origCast(process, serialized);
    };
    moduleInstance["call"] = (process, args) => {
        const serialized = moduleInstance.serialize(args);
        return origCall(process, serialized);
    };
    moduleInstance["onRunTrackedJs"] = (scriptString, isDebug) => {
        const trackValue = (tracked) => {
            const getKey = moduleInstance["nextTrackedObjectKey"];
            const map = moduleInstance["trackedObjectsMap"];
            if (tracked instanceof TrackedValue) {
                map.set(tracked.key, tracked.value);
                return tracked.key;
            }
            const key = getKey();
            map.set(key, tracked);
            return key;
        };
        let fn;
        try {
            const indirectEval = eval;
            fn = indirectEval(scriptString);
        }
        catch (e) {
            // TODO: send onEvalError for Popcorn object
            console.error(e);
            return null;
        }
        if (isDebug)
            ensureFunctionEval(fn);
        let result;
        try {
            result = fn?.(moduleInstance);
        }
        catch (e) {
            // TODO: send onEvalError for Popcorn object
            console.error(e);
            return null;
        }
        if (isDebug)
            ensureResultKeyList(result);
        return result?.map(trackValue) ?? [];
    };
    moduleInstance["onGetTrackedObjects"] = (keys) => {
        const getTrackedObject = (key) => {
            const serialize = moduleInstance["serialize"];
            const map = moduleInstance["trackedObjectsMap"];
            return serialize(map.get(key));
        };
        return keys.map(getTrackedObject);
    };
    moduleInstance["sendEvent"] = (eventName, payload) => {
        sendIframeResponse(MESSAGES.EVENT, { eventName, payload });
    };
}
async function handleCall(request) {
    if (!Module) {
        throw new Error("Module not initialized");
    }
    const { requestId, process, args } = request;
    sendIframeResponse(MESSAGES.CALL_ACK, { requestId });
    try {
        const result = await Module.call(process, args);
        sendIframeResponse(MESSAGES.CALL, {
            requestId,
            data: Module.deserialize(result),
        });
    }
    catch (error) {
        if (error == "noproc") {
            sendIframeResponse(MESSAGES.RELOAD, null);
            console.error("Runtime VM crashed, popcorn iframe reloaded.");
            return;
        }
        sendIframeResponse(MESSAGES.CALL, {
            requestId,
            error: Module.deserialize(error),
        });
    }
}
function handleCast(request) {
    if (!Module) {
        throw new Error("Module not initialized");
    }
    const { process, args } = request;
    Module.cast(process, args);
}
function ensureFunctionEval(maybeFunction) {
    if (typeof maybeFunction !== "function") {
        throw new Error("Script passed to onRunTrackedJs() is not wrapped in a function");
    }
}
function ensureResultKeyList(result) {
    if (!Array.isArray(result) && result !== undefined) {
        throw new Error("Script passed to onRunTrackedJs() returned invalid value, accepted values are arrays and undefined");
    }
}
// `json` selects the realm whose JSON.parse builds the result. By default that
// is the iframe's own JSON, but run_js passes the parent window's JSON so the
// deserialized terms (arrays/objects) belong to the parent realm — otherwise
// cross-realm arrays sent out of the iframe fail `instanceof Array` checks in
// the parent (e.g. phoenix_live_view's diff renderer). Tracked `popcorn_ref`
// values are resolved by reference and keep their own realm regardless.
function deserialize(message, json = JSON) {
    return json.parse(message, (_key, value) => {
        const isRef = typeof value === "object" &&
            value !== null &&
            Object.hasOwn(value, "popcorn_ref") &&
            Object.getOwnPropertyNames(value).length == 1;
        if (!isRef) {
            return value;
        }
        return Module?.trackedObjectsMap.get(value.popcorn_ref);
    });
}

export { initVm };
