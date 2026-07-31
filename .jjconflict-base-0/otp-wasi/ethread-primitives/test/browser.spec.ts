import { expect, test } from "@playwright/test";

test("runs OTP ethread primitives", async ({ page }) => {
  await page.goto("/");

  const prerequisites = await page.evaluate(() => ({
    crossOriginIsolated,
    sharedArrayBuffer: typeof SharedArrayBuffer !== "undefined",
    sharedMemory: new WebAssembly.Memory({ initial: 1, maximum: 1, shared: true }).buffer instanceof SharedArrayBuffer,
  }));
  expect(prerequisites).toEqual({ crossOriginIsolated: true, sharedArrayBuffer: true, sharedMemory: true });

  const moduleSurface = await page.evaluate(async () => {
    const response = await fetch("/program.wasm");
    const module = await WebAssembly.compile(await response.arrayBuffer());
    return {
      imports: WebAssembly.Module.imports(module).map(({ module, name, kind }) => `${module}.${name}:${kind}`),
      exports: WebAssembly.Module.exports(module).map(({ name, kind }) => `${name}:${kind}`),
    };
  });
  expect(moduleSurface.imports).toEqual([
    "env.memory:memory",
    "experiment.progress:function",
    "experiment.report:function",
    "wasi_snapshot_preview1.fd_close:function",
    "wasi_snapshot_preview1.fd_seek:function",
    "wasi_snapshot_preview1.fd_write:function",
    "wasi_snapshot_preview1.proc_exit:function",
    "wasi_snapshot_preview1.sched_yield:function",
    "wasi_snapshot_preview1.random_get:function",
    "wasi.thread-spawn:function",
  ]);
  expect(moduleSurface.exports).toEqual(["_start:function", "wasi_thread_start:function"]);

  const result = await page.evaluate(async () => await Promise.race([
    window.runWasiEthreadExperiment(),
    new Promise((resolve) => setTimeout(() => resolve({ timeout: true, progress: window.wasiEthreadProgress }), 10_000)),
  ]));
  expect(result).toEqual({
    ok: true,
    initialized: true,
    mutexCondition: true,
    event: true,
    atomics: 12,
    completed: 3,
    peakWorkers: 3,
    activeWorkers: 0,
  });

  const messages = await page.evaluate(async () => {
    const before = window.wasiEthreadMessageCount;
    await new Promise((resolve) => setTimeout(resolve, 100));
    return { before, after: window.wasiEthreadMessageCount };
  });
  expect(messages.before).toBeGreaterThan(1);
  expect(messages.after).toBe(messages.before);
});
