import { assert, evalOpts, expect, test } from "./helpers";

function evalElixirOpts(code: string) {
  const bytes = new TextEncoder().encode(code);
  return evalOpts(`
    {ok, _} = application:ensure_all_started(elixir),
    'Elixir.Code':eval_string(<<${bytes.join(",")}>>).
  `);
}

test("boots", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/assets/otp/manifest.json" },
  });

  expect(result).toEqual({ ok: true, data: null });
});

test("auto-starts the manifest entrypoint app", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/assets/otp/manifest.json" },
  });
  expect(result).toEqual({ ok: true, data: null });

  const event = await otp.waitForEvent("entrypoint_started");
  expect(event).toMatchObject({ entrypoint_started: true });
});

test("reports timeouts on init", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/assets/otp/manifest.json" },
    timeoutsMs: { boot: 0 },
  });

  expect(result).toEqual({
    ok: false,
    error: { t: "timeout:init", data: { timeoutMs: 0 } },
  });
});

test("handles missing manifest", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/assets/otp/missing/manifest.json" },
  });

  expect(result).toEqual({
    ok: false,
    error: {
      t: "beam:missing-manifest",
      data: { url: "/assets/otp/missing/manifest.json" },
    },
  });
});

test("failures on boot aren't sent to onEvent()", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const errors: unknown[] = [];
    const popcorn = new window.Popcorn({
      beam: { manifestUrl: "/assets/otp/missing/manifest.json" },
      onError: (event) => errors.push(event),
    });

    const boot = await popcorn.boot();
    return {
      errors,
      boot: boot.ok
        ? { ok: true, data: null }
        : { ok: false, error: boot.error.serialize() },
    };
  });

  expect(result.errors).toEqual([]);
  expect(result.boot).toEqual({
    ok: false,
    error: {
      t: "beam:missing-manifest",
      data: { url: "/assets/otp/missing/manifest.json" },
    },
  });
});

test("can reboot after deinit", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const popcorn = new window.Popcorn({
      beam: { manifestUrl: "/assets/otp/manifest.json" },
    });
    type BootResult = Awaited<ReturnType<typeof popcorn.boot>>;

    const first = await popcorn.boot();
    popcorn.deinit();
    const second = await popcorn.boot();
    popcorn.deinit();

    const serialize = (boot: BootResult) => {
      if (boot.ok) return { ok: true as const, data: null };
      return { ok: false as const, error: boot.error.serialize() };
    };

    return { first: serialize(first), second: serialize(second) };
  });

  expect(result.first).toEqual({ ok: true, data: null });
  expect(result.second).toEqual({ ok: true, data: null });
});

test("event hooks preserved after a reboot", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const READY_BOOT_EVAL = "ok = wasm:send(#{ready => true}).";
    const opts = {
      beam: {
        manifestUrl: "/assets/otp/manifest.json",
        extraArgs: ["-eval", READY_BOOT_EVAL],
      },
    };
    const hasReadyField = (event: unknown) =>
      typeof event === "object" &&
      event !== null &&
      Object.hasOwn(event, "ready");

    const events: unknown[] = [];
    let onReady: (() => void) | null = null;
    const popcorn = new window.Popcorn(opts);
    popcorn.onEvent((event) => {
      events.push(event);
      if (onReady && hasReadyField(event)) {
        onReady();
        onReady = null;
      }
    });

    const waitForReady = () =>
      new Promise<void>((resolve) => {
        if (events.some(hasReadyField)) {
          resolve();
          return;
        }
        onReady = resolve;
      });

    await popcorn.boot();
    await waitForReady();
    const afterFirstBoot = events.length;

    popcorn.deinit();
    events.length = 0;

    await popcorn.boot();
    await waitForReady();
    popcorn.deinit();

    return { afterFirstBoot, afterReboot: events.length };
  });

  expect(result.afterFirstBoot).toBeGreaterThan(0);
  expect(result.afterReboot).toBeGreaterThan(0);
});

test("failing boot fails immediately", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const popcorn = new window.Popcorn({
      beam: { manifestUrl: "/assets/otp/manifest.json" },
      workerUrl: "/fault-worker.mjs",
      timeoutsMs: { boot: 30_000 },
    });

    const startMs = performance.now();
    const boot = await popcorn.boot();
    const elapsedMs = performance.now() - startMs;
    popcorn.deinit();

    return {
      elapsedMs,
      boot: boot.ok
        ? { ok: true, data: null }
        : { ok: false, error: boot.error.serialize() },
    };
  });

  expect(result.boot).toEqual({
    ok: false,
    error: { t: "vm:exited", data: { reason: "abort", data: "boom" } },
  });
  // should be less than boot timeoutMs
  expect(result.elapsedMs).toBeLessThan(5_000);
});


test("loads manifest apps before eval", async ({ otp }) => {
  const boot = await otp.boot(
    evalOpts(`
      {ok, _} = application:ensure_all_started(elixir),
      ok = wasm:send(#{manifest_app => true}).
    `),
  );
  assert(boot.ok);

  await otp.waitForEvent("manifest_app");
  expect(otp.events).toContainEqual({ manifest_app: true });
});

test("evaluates Elixir code", async ({ otp }) => {
  const boot = await otp.boot(
    evalElixirOpts(`
      :ok = :wasm.send(%{elixir_result: 1 + 2})
    `),
  );
  assert(boot.ok);

  await otp.waitForEvent("elixir_result");
  expect(otp.events).toContainEqual({ elixir_result: 3 });
});

test("starts the provided logger app", async ({ otp }) => {
  const boot = await otp.boot(
    evalOpts(`
      {ok, _} = application:ensure_all_started(logger),
      _ = 'Elixir.Logger':level(),
      ok = wasm:send(#{logger_started => true}).
    `),
  );
  assert(boot.ok);

  await otp.waitForEvent("logger_started");
  expect(otp.events).toContainEqual({ logger_started: true });
});


