import type { ExperimentResult } from "./host";

type StartMessage = { type: "start"; module: WebAssembly.Module; memory: WebAssembly.Memory; ports: MessagePort[]; slots: SharedArrayBuffer };
type ChildMessage = { type: "complete"; tid: number } | { type: "failure"; tid: number; error: string };

class WasiExit extends Error {
  constructor(readonly code: number) {
    super(`WASI exited with ${code}`);
  }
}

let module: WebAssembly.Module;
let memory: WebAssembly.Memory;
let nextTid = 1;
let ports: MessagePort[];
let slots: Int32Array;
let nextSlot = 0;
const liveTids = new Set<number>();
let result: Omit<ExperimentResult, "peakWorkers" | "activeWorkers"> | undefined;

self.addEventListener("message", async ({ data }: MessageEvent<StartMessage>) => {
  try {
    module = data.module;
    memory = data.memory;
    ports = data.ports;
    slots = new Int32Array(data.slots);
    for (const port of ports) {
      port.addEventListener("message", ({ data }: MessageEvent<ChildMessage>) => {
        if (data.type === "complete") liveTids.delete(data.tid);
        else fail(new Error(data.error));
      });
      port.start();
    }
    const instance = await WebAssembly.instantiate(module, imports());
    (instance.exports._start as () => void)();
    await finish();
  } catch (error) {
    if (error instanceof WasiExit && error.code === 0) await finish();
    else fail(error);
  }
});

function imports(): WebAssembly.Imports {
  return {
    env: { memory },
    wasi_snapshot_preview1: wasiImports(),
    wasi: { "thread-spawn": (startArg: number) => spawn(startArg) },
    experiment: {
      report(status: number, phase: number, counter: number, completed: number) {
        if (phase === 2) result = { ok: status === 0, phase, counter, completed };
      },
    },
  };
}

function wasiImports() {
  return {
    clock_time_get(_id: number, _precision: bigint, time: number) {
      new DataView(memory.buffer).setBigUint64(time, 0n, true);
      return 0;
    },
    fd_close: () => 0,
    fd_seek(_fd: number, _offset: bigint, _whence: number, result: number) {
      new DataView(memory.buffer).setBigUint64(result, 0n, true);
      return 0;
    },
    fd_write(_fd: number, _iovecs: number, _count: number, result: number) {
      new DataView(memory.buffer).setUint32(result, 0, true);
      return 0;
    },
    proc_exit(code: number) {
      throw new WasiExit(code);
    },
    sched_yield: () => 0,
  };
}

function spawn(startArg: number): number {
  const tid = nextTid++;
  const slot = claimSlot();
  if (slot === -1) return -1;
  const port = ports[slot];
  liveTids.add(tid);
  port.postMessage({ type: "start", module, memory, tid, startArg });
  return tid;
}

function claimSlot(): number {
  for (let offset = 0; offset < slots.length; offset += 1) {
    const slot = (nextSlot + offset) % slots.length;
    if (Atomics.compareExchange(slots, slot, 0, 1) === 0) {
      nextSlot = (slot + 1) % slots.length;
      return slot;
    }
  }
  return -1;
}

async function finish(): Promise<void> {
  await waitForThreads();
  if (!result) throw new Error("program exited without a structured result");
  self.postMessage({ type: "result", result: { ...result, peakWorkers: ports.length, activeWorkers: 0 } });
}

function waitForThreads(): Promise<void> {
  return new Promise((resolve) => {
    const interval = setInterval(() => {
      if (liveTids.size === 0) {
        clearInterval(interval);
        resolve();
      }
    }, 1);
  });
}

function fail(error: unknown): void {
  liveTids.clear();
  self.postMessage({ type: "failure", error: error instanceof Error ? error.message : String(error) });
}
