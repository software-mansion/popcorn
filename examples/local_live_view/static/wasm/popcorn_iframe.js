import init from "./AtomVM.mjs";

const MESSAGES = {
  INIT: "popcorn-init",
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
  RELOAD: "popcorn-reload",
};

const HEARTBEAT_INTERVAL_MS = 500;

let Module = null;

class TrackedValue {
  constructor({ key, value }) {
    if (typeof key !== "number") {
      throw new Error("key property in TrackedValue must be a number");
    }
    this.key = key;
    this.value = value;
  }
}

globalThis.TrackedValue = TrackedValue;

export async function initVm() {
  const bundlePath = document.querySelector('meta[name="bundle-path"]').content;

  Module = await init({
    arguments: [bundlePath],
    print(text) {
      send(MESSAGES.STDOUT, text);
    },
    printErr(text) {
      send(MESSAGES.STDERR, text);
    },
  });

  Module["serialize"] = JSON.stringify;
  Module["deserialize"] = deserialize;
  Module["cleanupFunctions"] = new Map();
  Module["onTrackedObjectDelete"] = (key) => {
    const fns = Module["cleanupFunctions"];
    const fn = fns.get(key);
    fns.delete(key);
    try {
      fn?.();
    } catch (e) {
      console.error(e);
    } finally {
      Module["trackedObjectsMap"].delete(key);
    }
  };
  const origCast = Module["cast"];
  const origCall = Module["call"];

  Module["cast"] = (process, args) => {
    const serialized = Module.serialize(args);
    origCast(process, serialized);
  };
  Module["call"] = (process, args) => {
    const serialized = Module.serialize(args);
    return origCall(process, serialized);
  };

  Module["onRunTrackedJs"] = (scriptString, isDebug) => {
    const trackValue = (tracked) => {
      const getKey = Module["nextTrackedObjectKey"];
      const map = Module["trackedObjectsMap"];

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
    } catch (e) {
      // TODO: send onEvalError for Popcorn object
      console.error(e);
      return null;
    }
    isDebug && ensureFunctionEval(fn);
    let result;
    try {
      result = fn(Module);
    } catch (e) {
      // TODO: send onEvalError for Popcorn object
      console.error(e);
      return null;
    }
    isDebug && ensureResultKeyList(result);

    return result?.map(trackValue) ?? [];
  };
  Module["onGetTrackedObjects"] = (keys) => {
    const getTrackedObject = (key) => {
      const serialize = Module["serialize"];
      const map = Module["trackedObjectsMap"];

      return serialize(map.get(key));
    };
    return keys.map(getTrackedObject);
  };

  Module["onElixirReady"] = (initProcess) => {
    onVmInit(initProcess);
    Module["onElixirReady"] = null;
  };
}

function ensureFunctionEval(maybeFunction) {
  if (typeof maybeFunction !== "function") {
    throw new Error(
      "Script passed to onRunTrackedJs() is not wrapped in a function",
    );
  }
}

function ensureResultKeyList(result) {
  if (!Array.isArray(result) && result !== undefined) {
    throw new Error(
      "Script passed to onRunTrackedJs() returned invalid value, accepted values are arrays and undefined",
    );
  }
}

function onVmInit(initProcess) {
  setInterval(() => send(MESSAGES.HEARTBEAT, null), HEARTBEAT_INTERVAL_MS);

  window.addEventListener("message", async ({ data }) => {
    const type = data.type;

    if (type === MESSAGES.CALL) {
      const { requestId, process, args } = data.value;
      send(MESSAGES.CALL_ACK, { requestId });

      try {
        const result = await Module.call(process, args);
        send(MESSAGES.CALL, { requestId, data: Module.deserialize(result) });
      } catch (error) {
        if(error == "noproc") {
          send(MESSAGES.RELOAD, null);
          console.error("Runtime VM crashed, popcorn iframe reloaded.");
          return;
        }
        send(MESSAGES.CALL, { requestId, error: Module.deserialize(error) });
      }
    } else if (type.startsWith("popcorn")) {
      `Iframe: received unhandled popcorn event: ${JSON.stringify(data, null, 4)}`;
    }
  });
  send(MESSAGES.INIT, initProcess);
}

function send(type, data) {
  window.parent.postMessage({ type, value: data });
}

function deserialize(message) {
  return JSON.parse(message, (key, value) => {
    const isRef =
      typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "popcorn_ref") &&
      Object.getOwnPropertyNames(value).length == 1;

    if (!isRef) {
      return value;
    }
    return Module.trackedObjectsMap.get(value.popcorn_ref);
  });
}
