import { expect, test } from "@playwright/test";

test("initializes real ERTS scheduling without child threads", async ({ page }) => {
  await page.goto("/");
  expect(await page.evaluate(() => ({
    crossOriginIsolated,
    sharedArrayBuffer: typeof SharedArrayBuffer !== "undefined",
  }))).toEqual({ crossOriginIsolated: true, sharedArrayBuffer: true });

  const surface = await page.evaluate(async () => {
    const module = await WebAssembly.compile(await (await fetch("/program.wasm")).arrayBuffer());
    return {
      imports: WebAssembly.Module.imports(module).map(({ module, name, kind }) => `${module}.${name}:${kind}`),
      exports: WebAssembly.Module.exports(module).map(({ name, kind }) => `${name}:${kind}`),
    };
  });
  expect(surface.imports).toEqual([
    "env.memory:memory",
    "experiment.progress:function",
    "experiment.report:function",
    "wasi_snapshot_preview1.clock_res_get:function",
    "wasi_snapshot_preview1.clock_time_get:function",
    "wasi_snapshot_preview1.fd_close:function",
    "wasi_snapshot_preview1.fd_fdstat_get:function",
    "wasi_snapshot_preview1.fd_fdstat_set_flags:function",
    "wasi_snapshot_preview1.fd_read:function",
    "wasi_snapshot_preview1.fd_seek:function",
    "wasi_snapshot_preview1.fd_write:function",
    "wasi_snapshot_preview1.poll_oneoff:function",
    "wasi_snapshot_preview1.proc_exit:function",
    "wasi_snapshot_preview1.sched_yield:function",
  ]);
  expect(surface.exports).toEqual(["_start:function", "wasi_thread_start:function"]);

  expect(await page.evaluate(() => window.runWasiErtsSchedulingInitialization())).toEqual({
    normalSchedulers: 1,
    dirtyCpuSchedulers: 1,
    dirtyIoSchedulers: 1,
    pollThreads: 1,
    runQueues: 1,
    totalBrowserWorkers: 1,
    peakActiveWorkers: 1,
    activeWorkersAfterTeardown: 0,
    threadSpawnCalls: 0,
    progress: [
      "ethread initialized",
      "allocator initialized",
      "check-I/O initialized",
      "thread progress initialized",
      "monitor/process-signal infrastructure initialized",
      "time initialized",
      "process table initialized",
      "entered erts_init_scheduling",
      "returned from erts_init_scheduling",
    ],
  });
});
