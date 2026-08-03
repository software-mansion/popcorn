import type { SchedulingResult } from "./host";
import { WasiExit, wasiImports } from "./wasi-imports";

type StartMessage = {
  type: "start";
  module: WebAssembly.Module;
  memory: WebAssembly.Memory;
};

const stages = [
  "ethread initialized",
  "allocator initialized",
  "thread progress initialized",
  "check-I/O initialized",
  "process table initialized",
  "time initialized",
  "monitor/process-signal infrastructure initialized",
  "entered erts_init_scheduling",
  "returned from erts_init_scheduling",
] as const;

self.addEventListener("message", async ({ data }: MessageEvent<StartMessage>) => {
  let counts: Pick<SchedulingResult, "normalSchedulers" | "dirtyCpuSchedulers" |
    "dirtyIoSchedulers" | "pollThreads" | "runQueues"> | undefined;
  try {
    const instance = await WebAssembly.instantiate(data.module, {
      env: { memory: data.memory },
      wasi_snapshot_preview1: wasiImports(data.memory),
      experiment: {
        progress(stage: number) {
          self.postMessage({ type: "progress", stage: stages[stage] });
        },
        report(normalSchedulers: number, dirtyCpuSchedulers: number,
               dirtyIoSchedulers: number, pollThreads: number, runQueues: number) {
          counts = { normalSchedulers, dirtyCpuSchedulers, dirtyIoSchedulers, pollThreads, runQueues };
        },
      },
    });
    (instance.exports._start as () => void)();
  } catch (error) {
    if (!(error instanceof WasiExit && error.code === 0)) return fail(error);
  }
  if (!counts) return fail(new Error("program exited without a structured result"));
  self.postMessage({ type: "result", counts });
});

function fail(error: unknown): void {
  self.postMessage({ type: "failure", error: error instanceof Error ? error.stack ?? error.message : String(error) });
}
