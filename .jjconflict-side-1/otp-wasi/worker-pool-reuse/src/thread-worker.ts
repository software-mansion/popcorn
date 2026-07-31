type StartMessage = {
  type: "start";
  module: WebAssembly.Module;
  memory: WebAssembly.Memory;
  tid: number;
  startArg: number;
};

type Message = { type: "initialize" } | { type: "connect"; port: MessagePort; slots: SharedArrayBuffer; slot: number };

self.addEventListener("message", ({ data }: MessageEvent<Message>) => {
  if (data.type === "initialize") {
    self.postMessage({ type: "ready" });
    return;
  }

  const port = data.port;
  const slot = data.slot;
  const slots = new Int32Array(data.slots);
  port.addEventListener("message", ({ data }: MessageEvent<StartMessage>) => start(data, port, slots, slot));
  port.start();
});

async function start(data: StartMessage, port: MessagePort, slots: Int32Array, slot: number): Promise<void> {
  try {
    const instance = await WebAssembly.instantiate(data.module, {
      env: { memory: data.memory },
      wasi_snapshot_preview1: wasiImports(data.memory),
      wasi: { "thread-spawn": () => -1 },
      experiment: { report: () => undefined },
    });
    (instance.exports.wasi_thread_start as (tid: number, startArg: number) => void)(data.tid, data.startArg);
    Atomics.store(slots, slot, 0);
    Atomics.notify(slots, slot);
    port.postMessage({ type: "complete", tid: data.tid });
  } catch (error) {
    Atomics.store(slots, slot, 0);
    Atomics.notify(slots, slot);
    port.postMessage({ type: "failure", tid: data.tid, error: error instanceof Error ? error.message : String(error) });
  }
}

function wasiImports(memory: WebAssembly.Memory) {
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
    proc_exit: () => undefined,
    sched_yield: () => 0,
  };
}
