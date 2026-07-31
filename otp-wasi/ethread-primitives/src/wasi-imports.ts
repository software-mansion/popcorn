export class WasiExit extends Error {
  constructor(readonly code: number) {
    super(`WASI exited with ${code}`);
  }
}

export function wasiImports(memory: WebAssembly.Memory) {
  return {
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
    random_get(buffer: number, length: number) {
      new Uint8Array(memory.buffer, buffer, length).fill(0xa5);
      return 0;
    },
    sched_yield: () => 0,
  };
}
