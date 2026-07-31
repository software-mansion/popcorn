export type TopologyResult = {
  ok: boolean;
  requiredChildren: number;
  completed: number;
  distinctIdentities: boolean;
  distinctTsd: boolean;
  synchronized: boolean;
  capacityError: number;
  totalPrewarmedWorkers: number;
  peakWorkers: number;
  activeWorkers: number;
  reusedWorkers: number;
};

const childWorkerCount = 7;

type VmMessage =
  | { type: "result"; result: TopologyResult }
  | { type: "failure"; error: string }
  | { type: "progress"; source: string; role: number; phase: number };

declare global {
  interface Window {
    runWasiErtsTopologyExperiment: () => Promise<TopologyResult>;
    wasiErtsTopologyProgress: string[];
    wasiErtsTopologyMessageCount: number;
  }
}

export async function runWasiErtsTopologyExperiment(): Promise<TopologyResult> {
  if (!crossOriginIsolated) throw new Error("cross-origin isolation is required");
  if (typeof SharedArrayBuffer === "undefined") throw new Error("SharedArrayBuffer is unavailable");

  const memory = new WebAssembly.Memory({ initial: 16, maximum: 256, shared: true });
  const response = await fetch("/program.wasm");
  if (!response.ok) throw new Error(`failed to load program.wasm: ${response.status}`);

  const module = await WebAssembly.compile(await response.arrayBuffer());
  const threadWorkers = await createThreadWorkers();
  const vmWorker = new Worker(new URL("./vm-worker.ts", import.meta.url), { type: "module" });
  window.wasiErtsTopologyProgress = [];
  window.wasiErtsTopologyMessageCount = 0;

  return await new Promise<TopologyResult>((resolve, reject) => {
    const stop = () => {
      vmWorker.terminate();
      for (const worker of threadWorkers) worker.terminate();
    };
    const handle = (data: VmMessage) => {
      window.wasiErtsTopologyMessageCount += 1;
      if (data.type === "progress") {
        window.wasiErtsTopologyProgress.push(`${data.source}:${data.role}:${data.phase}`);
        return;
      }
      stop();
      if (data.type === "result") resolve(data.result);
      else reject(new Error(data.error));
    };

    vmWorker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => handle(data));
    vmWorker.addEventListener("error", (event) => {
      stop();
      reject(event.error ?? new Error(event.message));
    });
    for (const threadWorker of threadWorkers) {
      threadWorker.addEventListener("message", ({ data }: MessageEvent<VmMessage>) => {
        if (data.type === "progress") handle(data);
      });
    }

    const channels = threadWorkers.map(() => new MessageChannel());
    for (let index = 0; index < threadWorkers.length; index += 1) {
      threadWorkers[index].postMessage({ type: "connect", port: channels[index].port1 }, [channels[index].port1]);
    }
    const ports = channels.map(({ port2 }) => port2);
    vmWorker.postMessage({ type: "start", module, memory, ports }, ports);
  });
}

window.runWasiErtsTopologyExperiment = runWasiErtsTopologyExperiment;

async function createThreadWorkers(): Promise<Worker[]> {
  const workers = Array.from({ length: childWorkerCount }, () =>
    new Worker(new URL("./thread-worker.ts", import.meta.url), { type: "module" }));
  await Promise.all(workers.map((worker) => new Promise<void>((resolve, reject) => {
    worker.addEventListener("message", ({ data }: MessageEvent<{ type: string }>) => {
      if (data.type === "ready") resolve();
    }, { once: true });
    worker.addEventListener("error", (event) => reject(event.error ?? new Error(event.message)), { once: true });
    worker.postMessage({ type: "initialize" });
  })));
  return workers;
}
