import { expect, test } from "@playwright/test";

test("runs pthreads in nested workers", async ({ page }) => {
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
  expect(moduleSurface.imports).toContain("env.memory:memory");
  expect(moduleSurface.imports).toContain("wasi.thread-spawn:function");
  expect(moduleSurface.exports).toEqual(expect.arrayContaining(["_start:function", "wasi_thread_start:function"]));

  const result = await page.evaluate(() => window.runWasiThreadsExperiment());
  expect(result).toEqual({
    ok: true,
    phase: 2,
    counter: 3,
    completed: 103,
    peakWorkers: 103,
    activeWorkers: 0,
  });

  const messages = await page.evaluate(async () => {
    const before = window.wasiThreadsMessageCount;
    await new Promise((resolve) => setTimeout(resolve, 100));
    return { before, after: window.wasiThreadsMessageCount };
  });
  expect(messages).toEqual({ before: 1, after: 1 });
});
