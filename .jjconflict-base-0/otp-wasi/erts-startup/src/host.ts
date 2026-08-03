export type SchedulingResult = {
  normalSchedulers: number;
  dirtyCpuSchedulers: number;
  dirtyIoSchedulers: number;
  pollThreads: number;
  runQueues: number;
  totalBrowserWorkers: number;
  peakActiveWorkers: number;
  activeWorkersAfterTeardown: number;
  threadSpawnCalls: number;
  progress: string[];
};

type VmMessage =
  | { type: "progress"; stage: string }
  | { type: "result"; counts: Pick<SchedulingResult, "normalSchedulers" |
      "dirtyCpuSchedulers" | "dirtyIoSchedulers" | "pollThreads" | "runQueues"> }
  | { type: "failure"; error: string };

declare global {
  interface Window {
    runWasiErtsSchedulingInitialization: () => Promise<SchedulingResult>;
  }
}

export async function runWasiErtsSchedulingInitialization(): Promise<SchedulingResult> {
  if (!crossOriginIsolated) throw new Error("cross-origin isolation is required");
  const response = await fetch("/program.wasm");
  if (!response.ok) throw new Error(`failed to load program.wasm: ${response.status}`);

  const module = await WebAssembly.compile(await response.arrayBuffer());
  const memory = new WebAssembly.Memory({ initial: 4, maximum: 256, shared: true });
  const worker = new Worker(new URL("./vm-worker.ts", import.meta.url), { type: "module" });
  const progress: string[] = [];

  return await new Promise<SchedulingResult>((resolve, reject) => {
    const stop = () => worker.terminate();
    worker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => {
      if (data.type === "progress") {
        progress.push(data.stage);
        return;
      }
      stop();
      if (data.type === "failure") {
        reject(new Error(`${data.error}; progress: ${progress.join(", ")}`));
        return;
      }
      resolve({
        ...data.counts,
        totalBrowserWorkers: 1,
        peakActiveWorkers: 1,
        activeWorkersAfterTeardown: 0,
        threadSpawnCalls: 0,
        progress,
      });
    });
    worker.addEventListener("error", (event) => {
      stop();
      reject(event.error ?? new Error(event.message));
    });
    worker.postMessage({ type: "start", module, memory });
  });
}

window.runWasiErtsSchedulingInitialization = runWasiErtsSchedulingInitialization;
