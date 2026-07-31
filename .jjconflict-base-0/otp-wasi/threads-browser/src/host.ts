export type ExperimentResult = {
  ok: boolean;
  phase: number;
  counter: number;
  completed: number;
  peakWorkers: number;
  activeWorkers: number;
  error?: string;
};

const workerCount = 103;

type VmMessage =
  | { type: "result"; result: ExperimentResult }
  | { type: "failure"; error: string };

declare global {
  interface Window {
    runWasiThreadsExperiment: () => Promise<ExperimentResult>;
    wasiThreadsMessageCount: number;
  }
}

export async function runWasiThreadsExperiment(): Promise<ExperimentResult> {
  if (!crossOriginIsolated) throw new Error("cross-origin isolation is required");
  if (typeof SharedArrayBuffer === "undefined") throw new Error("SharedArrayBuffer is unavailable");

  const memory = new WebAssembly.Memory({ initial: 16, maximum: 256, shared: true });
  if (!(memory.buffer instanceof SharedArrayBuffer)) throw new Error("shared WebAssembly.Memory is unavailable");

  const response = await fetch("/program.wasm");
  if (!response.ok) throw new Error(`failed to load program.wasm: ${response.status}`);

  const module = await WebAssembly.compile(await response.arrayBuffer());
  const worker = new Worker(new URL("./vm-worker.ts", import.meta.url), { type: "module" });
  window.wasiThreadsMessageCount = 0;

  return await new Promise<ExperimentResult>((resolve, reject) => {
    worker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => {
      window.wasiThreadsMessageCount += 1;
      worker.terminate();
      if (data.type === "result") resolve(data.result);
      else reject(new Error(data.error));
    });
    worker.addEventListener("error", (event) => {
      worker.terminate();
      reject(event.error ?? new Error(event.message));
    });
    worker.postMessage({ type: "start", module, memory });
  });
}

window.runWasiThreadsExperiment = runWasiThreadsExperiment;
