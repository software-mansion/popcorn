export type ExperimentResult = {
  ok: boolean;
  phase: number;
  counter: number;
  completed: number;
  peakWorkers: number;
  activeWorkers: number;
  error?: string;
};

const workerCount = 8;

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
  const threadWorkers = await createThreadWorkers();
  const slots = new SharedArrayBuffer(Int32Array.BYTES_PER_ELEMENT * workerCount);
  const worker = new Worker(new URL("./vm-worker.ts", import.meta.url), { type: "module" });
  window.wasiThreadsMessageCount = 0;

  return await new Promise<ExperimentResult>((resolve, reject) => {
    worker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => {
      window.wasiThreadsMessageCount += 1;
      worker.terminate();
      for (const threadWorker of threadWorkers) threadWorker.terminate();
      if (data.type === "result") resolve(data.result);
      else reject(new Error(data.error));
    });
    worker.addEventListener("error", (event) => {
      worker.terminate();
      for (const threadWorker of threadWorkers) threadWorker.terminate();
      reject(event.error ?? new Error(event.message));
    });
    const channels = threadWorkers.map(() => new MessageChannel());
    for (let index = 0; index < threadWorkers.length; index += 1) {
      threadWorkers[index].postMessage({ type: "connect", port: channels[index].port1, slots, slot: index }, [channels[index].port1]);
    }
    worker.postMessage({ type: "start", module, memory, ports: channels.map((channel) => channel.port2), slots }, channels.map((channel) => channel.port2));
  });
}

window.runWasiThreadsExperiment = runWasiThreadsExperiment;

async function createThreadWorkers(): Promise<Worker[]> {
  const workers = Array.from({ length: workerCount }, () => new Worker(new URL("./thread-worker.ts", import.meta.url), { type: "module" }));
  await Promise.all(workers.map((worker) => new Promise<void>((resolve, reject) => {
    worker.addEventListener("message", ({ data }: MessageEvent<{ type: string }>) => {
      if (data.type === "ready") resolve();
    }, { once: true });
    worker.addEventListener("error", (event) => reject(event.error ?? new Error(event.message)), { once: true });
    worker.postMessage({ type: "initialize" });
  })));
  return workers;
}
