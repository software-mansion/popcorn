import type { ExperimentResult } from "./host";

type StartMessage = { type: "start"; module: WebAssembly.Module; memory: WebAssembly.Memory };
type ChildMessage = { type: "complete"; tid: number } | { type: "failure"; tid: number; error: string };

class WasiExit extends Error {
  constructor(readonly code: number) {
    super(`WASI exited with ${code}`);
  }
}

let module: WebAssembly.Module;
let memory: WebAssembly.Memory;
let nextTid = 1;
const workers: Worker[] = [];
const liveTids = new Set<number>();
let result: Omit<ExperimentResult, "peakWorkers" | "activeWorkers"> | undefined;

self.addEventListener("message", async ({ data }: MessageEvent<StartMessage>) => {
  try {
    module = data.module;
    memory = data.memory;
    await startWorkers();
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
  const worker = workers[tid - 1];
  liveTids.add(tid);
  worker.postMessage({ type: "start", module, memory, tid, startArg });
  return tid;
}

async function finish(): Promise<void> {
  await waitForThreads();
  if (!result) throw new Error("program exited without a structured result");
  for (const worker of workers) worker.terminate();
  self.postMessage({ type: "result", result: { ...result, peakWorkers: workers.length, activeWorkers: 0 } });
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
  for (const worker of workers) worker.terminate();
  liveTids.clear();
  self.postMessage({ type: "failure", error: error instanceof Error ? error.message : String(error) });
}

async function startWorkers(): Promise<void> {
  await Promise.all(Array.from({ length: 103 }, () => new Promise<void>((resolve, reject) => {
    const worker = new Worker(new URL("./thread-worker.ts", import.meta.url), { type: "module" });
    workers.push(worker);
    worker.addEventListener("message", ({ data }: MessageEvent<ChildMessage | { type: "ready" }>) => {
      if (data.type === "ready") resolve();
      else if (data.type === "complete") liveTids.delete(data.tid);
      else fail(new Error(data.error));
    });
    worker.addEventListener("error", (event) => reject(event.error ?? new Error(event.message)));
    worker.postMessage({ type: "initialize" });
  })));
}
