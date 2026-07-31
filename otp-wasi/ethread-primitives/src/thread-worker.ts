import { wasiImports } from "./wasi-imports";

type StartMessage = {
  type: "start";
  module: WebAssembly.Module;
  memory: WebAssembly.Memory;
  tid: number;
  startArg: number;
};

type Message = { type: "initialize" } | { type: "connect"; port: MessagePort };

self.addEventListener("message", ({ data }: MessageEvent<Message>) => {
  if (data.type === "initialize") {
    self.postMessage({ type: "ready" });
    return;
  }

  const port = data.port;
  port.addEventListener("message", ({ data }: MessageEvent<StartMessage>) => start(data, port));
  port.start();
});

async function start(data: StartMessage, port: MessagePort): Promise<void> {
  try {
    const instance = await WebAssembly.instantiate(data.module, {
      env: { memory: data.memory },
      wasi_snapshot_preview1: wasiImports(data.memory),
      wasi: { "thread-spawn": () => -1 },
      experiment: {
        report: () => undefined,
        progress: (phase: number) => self.postMessage({ type: "progress", source: `thread-${data.tid}`, phase }),
      },
    });
    (instance.exports.wasi_thread_start as (tid: number, startArg: number) => void)(data.tid, data.startArg);
    port.postMessage({ type: "complete", tid: data.tid });
  } catch (error) {
    port.postMessage({ type: "failure", tid: data.tid, error: error instanceof Error ? error.message : String(error) });
  }
}
