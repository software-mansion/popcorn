import type { EthreadResult } from "./host";
import { WasiExit, wasiImports } from "./wasi-imports";

type StartMessage = { type: "start"; module: WebAssembly.Module; memory: WebAssembly.Memory; ports: MessagePort[] };
type ChildMessage = { type: "complete"; tid: number } | { type: "failure"; tid: number; error: string };

let module: WebAssembly.Module;
let memory: WebAssembly.Memory;
let ports: MessagePort[];
let nextTid = 1;
let nextWorker = 0;
const liveTids = new Set<number>();
let result: Omit<EthreadResult, "peakWorkers" | "activeWorkers"> | undefined;

self.addEventListener("message", async ({ data }: MessageEvent<StartMessage>) => {
  try {
    module = data.module;
    memory = data.memory;
    ports = data.ports;
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
    wasi_snapshot_preview1: wasiImports(memory),
    wasi: { "thread-spawn": (startArg: number) => spawn(startArg) },
    experiment: {
      progress(phase: number) {
        self.postMessage({ type: "progress", source: "vm", phase });
      },
      report(status: number, initialized: number, mutexCondition: number, event: number, atomics: number, completed: number) {
        result = {
          ok: status === 0,
          initialized: initialized === 1,
          mutexCondition: mutexCondition === 1,
          event: event === 1,
          atomics,
          completed,
        };
      },
    },
  };
}

function spawn(startArg: number): number {
  if (nextWorker === ports.length) return -1;
  const tid = nextTid++;
  liveTids.add(tid);
  ports[nextWorker++].postMessage({ type: "start", module, memory, tid, startArg });
  return tid;
}

async function finish(): Promise<void> {
  await waitForThreads();
  if (!result) throw new Error("program exited without a structured result");
  self.postMessage({ type: "result", result: { ...result, peakWorkers: nextWorker, activeWorkers: 0 } });
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
