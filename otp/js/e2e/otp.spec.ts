import { assert, expect, test } from "./helpers";

test("boots", async ({ otp }) => {
  const result = await otp.boot({ beam: { assetsUrl: "/otp-assets" } });

  expect(result).toEqual({ ok: true, data: null });
});

test("reports timeouts on init", async ({ otp }) => {
  const result = await otp.boot({
    beam: { assetsUrl: "/otp-assets" },
    timeoutsMs: { boot: 0 },
  });

  expect(result).toEqual({
    ok: false,
    error: { t: "timeout:init", data: { timeoutMs: 0 } },
  });
});

test("handles missing boot script", async ({ otp }) => {
  const result = await otp.boot({
    beam: { assetsUrl: "/otp-assets/missing" },
  });

  expect(result).toEqual({
    ok: false,
    error: {
      t: "beam:missing-boot-script",
      data: { url: "/otp-assets/missing/bin/vm.boot" },
    },
  });
});

test("failures on boot aren't sent to onEvent()", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const errors: unknown[] = [];
    const popcorn = new window.Popcorn({
      beam: { assetsUrl: "/otp-assets/missing" },
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
      t: "beam:missing-boot-script",
      data: { url: "/otp-assets/missing/bin/vm.boot" },
    },
  });
});

test("can reboot after deinit", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const popcorn = new window.Popcorn({ beam: { assetsUrl: "/otp-assets" } });
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
        assetsUrl: "/otp-assets",
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

test("failing boot fails fast (no timeout)", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const popcorn = new window.Popcorn({
      beam: { assetsUrl: "/otp-assets" },
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

test("handles events in both directions", async ({ otp }) => {
  const BRIDGE_BOOT_EVAL = [
    "spawn(fun() ->",
    "  ok = wasm:send(#{direct => true, nested => #{count => 1}}),",
    "  true = register(bridge, self()),",
    "  Loop = fun(F) ->",
    "    ok = wasm:send(#{bridge_ready => true}),",
    "    receive",
    "      {wasm, PayloadJson, MetaJson} ->",
    "        Payload = json:decode(PayloadJson),",
    "        Meta = json:decode(MetaJson),",
    "        ok = wasm:send(#{reply => Payload, meta => Meta})",
    "    after 100 ->",
    "        F(F)",
    "    end",
    "  end,",
    "  Loop(Loop)",
    "end).",
  ].join(" ");
  const boot = await otp.boot({
    beam: {
      assetsUrl: "/otp-assets",
      extraArgs: ["-eval", BRIDGE_BOOT_EVAL],
    },
  });

  assert(boot.ok);
  await otp.waitForEvent("direct");
  expect(otp.events).toContainEqual({
    direct: true,
    nested: { count: 1 },
  });

  await otp.waitForEvent("bridge_ready");

  const send = await otp.send(
    "bridge",
    { ping: true },
    { meta: { requestId: "req-1" } },
  );
  assert(send.ok);

  await otp.waitForEvent("reply");
  expect(otp.events).toContainEqual({
    reply: { ping: true },
    meta: { requestId: "req-1" },
  });
});

test("popcorn.send() reports unregistered process", async ({ otp }) => {
  const READY_BOOT_EVAL = "ok = wasm:send(#{ready => true}).";
  const boot = await otp.boot({
    beam: { assetsUrl: "/otp-assets", extraArgs: ["-eval", READY_BOOT_EVAL] },
  });
  assert(boot.ok);
  await otp.waitForEvent("ready");

  const send = await otp.send("missing-listener", { ping: true });
  expect(send).toEqual({
    ok: false,
    error: {
      t: "bridge:listener-not-found",
      data: { targetName: "missing-listener" },
    },
  });
});

test("reports timeouts on send", async ({ otp }) => {
  const boot = await otp.boot({
    beam: { assetsUrl: "/otp-assets" },
    timeoutsMs: { send: 0 },
  });
  assert(boot.ok);

  const send = await otp.send("any-target", { ping: true });
  expect(send).toEqual({
    ok: false,
    error: { t: "timeout:send", data: { timeoutMs: 0 } },
  });
});
