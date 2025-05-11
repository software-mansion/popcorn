import init from "./AtomVM.mjs";

const MESSAGES = {
  INIT: "fission-init",
  CALL: "fission-call",
  CAST: "fission-cast",
  CALL_ACK: "fission-callAck",
  STDOUT: "fission-stdout",
  STDERR: "fission-stderr",
  HEARTBEAT: "fission-heartbeat",
};

const HEARTBEAT_INTERVAL_MS = 500;

let Module = null;

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
  Module["onRemoteObjectDelete"] = (key) => {
    const fns = Module["cleanupFunctions"];
    const fn = fns.get(key);
    fns.delete(key);
    try {
      fn?.();
    } catch (e) {
      console.error(e);
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
  Module["onElixirReady"] = (initProcess) => {
    onVmInit(initProcess);
    Module["onElixirReady"] = null;
  };
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
        send(MESSAGES.CALL, { requestId, error: Module.deserialize(error) });
      }
    } else if (type.startsWith("fission")) {
      `Iframe: received unhandled fission event: ${JSON.stringify(data, null, 4)}`;
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
      Object.hasOwn(value, "fission_ref") &&
      Object.getOwnPropertyNames(value).length == 1;

    if (!isRef) {
      return value;
    }
    return Module.remoteObjectsMap.get(value.fission_ref);
  });
}
