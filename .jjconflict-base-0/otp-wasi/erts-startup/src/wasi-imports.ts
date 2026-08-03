export class WasiExit extends Error {
  constructor(readonly code: number) {
    super(`WASI exited with ${code}`);
  }
}

export function wasiImports(memory: WebAssembly.Memory) {
  const view = () => new DataView(memory.buffer);
  return {
    clock_res_get(_clock: number, result: number) {
      view().setBigUint64(result, 1_000n, true);
      return 0;
    },
    clock_time_get(_clock: number, _precision: bigint, result: number) {
      view().setBigUint64(result, BigInt(Date.now()) * 1_000_000n, true);
      return 0;
    },
    fd_close: () => 0,
    fd_fdstat_get(_fd: number, result: number) {
      new Uint8Array(memory.buffer, result, 24).fill(0);
      return 0;
    },
    fd_fdstat_set_flags: () => 0,
    fd_read(_fd: number, _iovecs: number, _count: number, result: number) {
      view().setUint32(result, 0, true);
      return 0;
    },
    fd_seek(_fd: number, _offset: bigint, _whence: number, result: number) {
      view().setBigUint64(result, 0n, true);
      return 0;
    },
    fd_write(_fd: number, _iovecs: number, _count: number, result: number) {
      view().setUint32(result, 0, true);
      return 0;
    },
    poll_oneoff(_subscriptions: number, _events: number, _count: number, result: number) {
      view().setUint32(result, 0, true);
      return 0;
    },
    proc_exit(code: number) {
      throw new WasiExit(code);
    },
    sched_yield: () => 0,
  };
}
