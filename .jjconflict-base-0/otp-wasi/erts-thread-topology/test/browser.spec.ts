import { expect, test } from "@playwright/test";

test("runs the bounded ERTS-shaped topology", async ({ page }) => {
  await page.goto("/");
  expect(await page.evaluate(() => ({
    crossOriginIsolated,
    sharedArrayBuffer: typeof SharedArrayBuffer !== "undefined",
  }))).toEqual({ crossOriginIsolated: true, sharedArrayBuffer: true });

  const moduleSurface = await page.evaluate(async () => {
    const module = await WebAssembly.compile(await (await fetch("/program.wasm")).arrayBuffer());
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
    window.runWasiErtsTopologyExperiment(),
    new Promise((resolve) => setTimeout(() => resolve({ timeout: true, progress: window.wasiErtsTopologyProgress }), 10_000)),
  ]));
  expect(result).toEqual({
    ok: true,
    requiredChildren: 6,
    completed: 7,
    distinctIdentities: true,
    distinctTsd: true,
    synchronized: true,
    capacityError: 6,
    totalPrewarmedWorkers: 8,
    peakWorkers: 8,
    activeWorkers: 0,
    reusedWorkers: 0,
  });

  const messages = await page.evaluate(async () => {
    const before = window.wasiErtsTopologyMessageCount;
    await new Promise((resolve) => setTimeout(resolve, 100));
    return { before, after: window.wasiErtsTopologyMessageCount };
  });
  expect(messages.before).toBeGreaterThan(14);
  expect(messages.after).toBe(messages.before);
});
