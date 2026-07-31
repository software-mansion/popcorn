export type EthreadResult = {
  ok: boolean;
  initialized: boolean;
  mutexCondition: boolean;
  event: boolean;
  atomics: number;
  completed: number;
  peakWorkers: number;
  activeWorkers: number;
  error?: string;
};

const workerCount = 3;

type VmMessage =
  | { type: "result"; result: EthreadResult }
  | { type: "failure"; error: string }
  | { type: "progress"; source: string; phase: number };

declare global {
  interface Window {
    runWasiEthreadExperiment: () => Promise<EthreadResult>;
    wasiEthreadMessageCount: number;
    wasiEthreadProgress: string[];
  }
}

export async function runWasiEthreadExperiment(): Promise<EthreadResult> {
  if (!crossOriginIsolated) throw new Error("cross-origin isolation is required");
  if (typeof SharedArrayBuffer === "undefined") throw new Error("SharedArrayBuffer is unavailable");

  const memory = new WebAssembly.Memory({ initial: 16, maximum: 256, shared: true });
  if (!(memory.buffer instanceof SharedArrayBuffer)) throw new Error("shared WebAssembly.Memory is unavailable");

  const response = await fetch("/program.wasm");
  if (!response.ok) throw new Error(`failed to load program.wasm: ${response.status}`);

  const module = await WebAssembly.compile(await response.arrayBuffer());
  const threadWorkers = await createThreadWorkers();
  const worker = new Worker(new URL("./vm-worker.ts", import.meta.url), { type: "module" });
  window.wasiEthreadMessageCount = 0;
  window.wasiEthreadProgress = [];

  for (const threadWorker of threadWorkers) {
    threadWorker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => {
      if (data.type === "progress") {
        window.wasiEthreadMessageCount += 1;
        window.wasiEthreadProgress.push(`${data.source}:${data.phase}`);
      }
    });
  }

  return await new Promise<EthreadResult>((resolve, reject) => {
    const stop = () => {
      worker.terminate();
      for (const threadWorker of threadWorkers) threadWorker.terminate();
    };

    worker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => {
      if (data.type === "progress") {
        window.wasiEthreadMessageCount += 1;
        window.wasiEthreadProgress.push(`${data.source}:${data.phase}`);
        return;
      }
      window.wasiEthreadMessageCount += 1;
      stop();
      if (data.type === "result") resolve(data.result);
      else reject(new Error(data.error));
    });
    worker.addEventListener("error", (event) => {
      stop();
      reject(event.error ?? new Error(event.message));
    });

    const channels = threadWorkers.map(() => new MessageChannel());
    for (let index = 0; index < threadWorkers.length; index += 1) {
      threadWorkers[index].postMessage({ type: "connect", port: channels[index].port1 }, [channels[index].port1]);
    }
    worker.postMessage({ type: "start", module, memory, ports: channels.map((channel) => channel.port2) }, channels.map((channel) => channel.port2));
  });
}

window.runWasiEthreadExperiment = runWasiEthreadExperiment;

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
