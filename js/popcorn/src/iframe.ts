// @ts-expect-error atomvm doesn't have types yet
import init from "./AtomVM.mjs";
import { sendIframeResponse } from "./bridge";
import { HEARTBEAT_INTERVAL_MS, MESSAGES } from "./types";

import type {
  AnySerializable,
  CallRequest,
  CastRequest,
  IframeRequest,
} from "./types";

/** Emscripten filesystem interface */
type EmscriptenFS = {
  mkdir: (path: string) => void;
  writeFile: (path: string, data: Int8Array) => void;
};

/** AtomVM Module interface - the WASM module with Elixir runtime */
type AtomVMModule = {
  serialize: (data: AnySerializable) => string;
  deserialize: (message: string) => AnySerializable;
  cleanupFunctions: Map<number, () => void>;
  trackedObjectsMap: Map<number, AnySerializable>;
  nextTrackedObjectKey: () => number;
  cast: (process: string, args: string) => void;
  call: (process: string, args: string) => Promise<string>;
  onTrackedObjectDelete: ((key: number) => void) | null;
  onRunTrackedJs:
    | ((scriptString: string, isDebug: boolean) => number[] | null)
    | null;
  onGetTrackedObjects: ((keys: number[]) => string[]) | null;
  onElixirReady: ((initProcess: string) => void) | null;
};

let Module: AtomVMModule | null = null;

class TrackedValue {
  key: number;
  value: AnySerializable;

  constructor({ key, value }: { key: number; value: AnySerializable }) {
    if (typeof key !== "number") {
      throw new Error("key property in TrackedValue must be a number");
    }
    this.key = key;
    this.value = value;
  }
}

declare const globalThis: { TrackedValue: typeof TrackedValue };
globalThis.TrackedValue = TrackedValue;

export async function runIFrame(): Promise<void> {
  const metaElement = document.querySelector(
    'meta[name="bundle-path"]',
  ) as HTMLMetaElement | null;
  if (!metaElement) {
    throw new Error("Missing meta[name='bundle-path'] element");
  }
  const bundlePath = metaElement.content;
  const bundleBuffer = await fetch(bundlePath).then((resp) =>
    resp.arrayBuffer(),
  );
  const bundle = new Int8Array(bundleBuffer);
  sendIframeResponse(MESSAGES.INIT, null);
  const initProcess = await startVm(bundle);

  window.addEventListener(
    "message",
    async ({ data }: MessageEvent<IframeRequest>) => {
      const type = data.type;

      if (type === MESSAGES.CALL) {
        await handleCall(data.value);
      } else if (type === MESSAGES.CAST) {
        handleCast(data.value);
      }
    },
  );

  sendIframeResponse(MESSAGES.START_VM, initProcess);
  setInterval(
    () => sendIframeResponse(MESSAGES.HEARTBEAT, null),
    HEARTBEAT_INTERVAL_MS,
  );
}

async function startVm(avmBundle: Int8Array): Promise<string> {
  let resolveResultPromise: ((value: string) => void) | null = null;
  const resultPromise = new Promise<string>((resolve) => {
    resolveResultPromise = resolve;
  });
  const moduleInstance: AtomVMModule = await init({
    preRun: [
      function ({ FS }: { FS: EmscriptenFS }) {
        FS.mkdir("/data");
        FS.writeFile("/data/bundle.avm", avmBundle);
      },
    ],
    arguments: ["/data/bundle.avm"],
    print(text: string) {
      sendIframeResponse(MESSAGES.STDOUT, text);
    },
    printErr(text: string) {
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
  moduleInstance["cleanupFunctions"] = new Map<number, () => void>();
  moduleInstance["onTrackedObjectDelete"] = (key: number) => {
    const fns = moduleInstance["cleanupFunctions"];
    const fn = fns.get(key);
    fns.delete(key);
    try {
      fn?.();
    } catch (e) {
      console.error(e);
    } finally {
      moduleInstance["trackedObjectsMap"].delete(key);
    }
  };
  const origCast = moduleInstance["cast"];
  const origCall = moduleInstance["call"];

  moduleInstance["cast"] = (process: string, args: AnySerializable) => {
    const serialized = moduleInstance.serialize(args);
    origCast(process, serialized);
  };
  moduleInstance["call"] = (process: string, args: AnySerializable) => {
    const serialized = moduleInstance.serialize(args);
    return origCall(process, serialized);
  };

  moduleInstance["onRunTrackedJs"] = (
    scriptString: string,
    isDebug: boolean,
  ) => {
    const trackValue = (tracked: AnySerializable): number => {
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

    let fn:
      | ((module: AtomVMModule) => AnySerializable[] | undefined)
      | undefined;
    try {
      const indirectEval = eval;
      fn = indirectEval(scriptString);
    } catch (e) {
      // TODO: send onEvalError for Popcorn object
      console.error(e);
      return null;
    }
    if (isDebug) ensureFunctionEval(fn);
    let result: AnySerializable[] | undefined;
    try {
      result = fn?.(moduleInstance);
    } catch (e) {
      // TODO: send onEvalError for Popcorn object
      console.error(e);
      return null;
    }
    if (isDebug) ensureResultKeyList(result);

    return result?.map(trackValue) ?? [];
  };
  moduleInstance["onGetTrackedObjects"] = (keys: number[]) => {
    const getTrackedObject = (key: number): string => {
      const serialize = moduleInstance["serialize"];
      const map = moduleInstance["trackedObjectsMap"];

      return serialize(map.get(key));
    };
    return keys.map(getTrackedObject);
  };

  moduleInstance["onElixirReady"] = (initProcess: string) => {
    moduleInstance["onElixirReady"] = null;
    resolveResultPromise?.(initProcess);
  };

  return resultPromise;
}

async function handleCall(request: CallRequest): Promise<void> {
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
  } catch (error) {
    if (error == "noproc") {
      sendIframeResponse(MESSAGES.RELOAD, null);
      console.error("Runtime VM crashed, popcorn iframe reloaded.");
      return;
    }
    sendIframeResponse(MESSAGES.CALL, {
      requestId,
      error: Module.deserialize(error as string),
    });
  }
}

function handleCast(request: CastRequest): void {
  if (!Module) {
    throw new Error("Module not initialized");
  }

  const { process, args } = request;
  Module.cast(process, args);
}

function ensureFunctionEval(maybeFunction: unknown): void {
  if (typeof maybeFunction !== "function") {
    throw new Error(
      "Script passed to onRunTrackedJs() is not wrapped in a function",
    );
  }
}

function ensureResultKeyList(result: unknown): void {
  if (!Array.isArray(result) && result !== undefined) {
    throw new Error(
      "Script passed to onRunTrackedJs() returned invalid value, accepted values are arrays and undefined",
    );
  }
}

function deserialize(message: string): AnySerializable {
  return JSON.parse(message, (_key: string, value: AnySerializable) => {
    const isRef =
      typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "popcorn_ref") &&
      Object.getOwnPropertyNames(value).length == 1;

    if (!isRef) {
      return value;
    }
    return Module?.trackedObjectsMap.get(value.popcorn_ref);
  });
}
