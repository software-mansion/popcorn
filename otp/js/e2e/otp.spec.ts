import {
  assert,
  expect,
  finishRunJs,
  getCleanups,
  test,
  trimLeft,
  valuesWithKey,
  waitForRunJsSuspension,
} from "./helpers";

function evalOpts(code: string) {
  return {
    beam: {
      manifestUrl: "/otp-assets/manifest.json",
      extraArgs: ["-eval", code],
    },
  };
}

test("boots", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/otp-assets/manifest.json" },
  });

  expect(result).toEqual({ ok: true, data: null });
});

test("auto-starts the manifest entrypoint app", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/otp-assets/manifest-entrypoint.json" },
  });
  expect(result).toEqual({ ok: true, data: null });

  const event = await otp.waitForEvent("entrypoint_started");
  expect(event).toMatchObject({ entrypoint_started: true });
});

test("reports timeouts on init", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/otp-assets/manifest.json" },
    timeoutsMs: { boot: 0 },
  });

  expect(result).toEqual({
    ok: false,
    error: { t: "timeout:init", data: { timeoutMs: 0 } },
  });
});

test("handles missing manifest", async ({ otp }) => {
  const result = await otp.boot({
    beam: { manifestUrl: "/otp-assets/missing/manifest.json" },
  });

  expect(result).toEqual({
    ok: false,
    error: {
      t: "beam:missing-manifest",
      data: { url: "/otp-assets/missing/manifest.json" },
    },
  });
});

test("failures on boot aren't sent to onEvent()", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const errors: unknown[] = [];
    const popcorn = new window.Popcorn({
      beam: { manifestUrl: "/otp-assets/missing/manifest.json" },
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
      data: { url: "/otp-assets/missing/manifest.json" },
    },
  });
});

test("can reboot after deinit", async ({ page }) => {
  const result = await page.evaluate(async () => {
    const popcorn = new window.Popcorn({
      beam: { manifestUrl: "/otp-assets/manifest.json" },
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
        manifestUrl: "/otp-assets/manifest.json",
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
      beam: { manifestUrl: "/otp-assets/manifest.json" },
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
  const BRIDGE_BOOT_EVAL = trimLeft(`
    spawn(fun() ->
      ok = wasm:send(#{direct => true, nested => #{count => 1}}),
      true = register(bridge, self()),
      Loop = fun(F) ->
        ok = wasm:send(#{bridge_ready => true}),
        receive
          {wasm, PayloadJson, MetaJson} ->
            Payload = json:decode(PayloadJson),
            Meta = json:decode(MetaJson),
            ok = wasm:send(#{reply => Payload, meta => Meta})
        after 100 ->
            F(F)
        end
      end,
      Loop(Loop)
    end).
  `);
  const boot = await otp.boot(evalOpts(BRIDGE_BOOT_EVAL));

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

test("run_js -> send", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    V = wasm:run_js(<<"(args) => 1 + 2">>, #{}),
    ok = wasm:send(#{run_js_result => V}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("run_js_result");
  expect(otp.events).toContainEqual({ run_js_result: 3 });
});

test("async run_js", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    V = wasm:run_js(<<"async ({a, b}) => a + b">>, #{a => 2, b => 5}),
    ok = wasm:send(#{run_js_async => V}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("run_js_async");
  expect(otp.events).toContainEqual({ run_js_async: 7 });
});

test("run_js timeout", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    R = try
      wasm:run_js(<<"() => new Promise(() => {})">>, #{}, [{timeout, 0}])
    catch
      error:run_js_timeout -> <<"timeout">>
    end,
    ok = wasm:send(#{run_js_timeout => R}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("run_js_timeout");
  expect(otp.events).toContainEqual({ run_js_timeout: "timeout" });
});

test("throwing run_js raises in VM", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    R = try
      wasm:run_js(<<"() => { throw new Error('boom') }">>, #{})
    catch
      error:{run_js, Msg} -> Msg
    end,
    ok = wasm:send(#{run_js_error => R}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("run_js_error");
  expect(otp.events).toContainEqual({ run_js_error: "Error: boom" });
});

test("tracked values keep identity", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    H = wasm:run_js(<<"() => new TrackedValue({n: 1})">>, #{}),
    V = wasm:run_js(<<"({h}) => { h.n = h.n + 1; return h.n; }">>, #{h => H}),
    ok = wasm:send(#{roundtrip => V}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("roundtrip");
  expect(otp.events).toContainEqual({ roundtrip: 2 });
});

test("nested tracked values", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    H = wasm:run_js(<<"() => new TrackedValue({n: 5})">>, #{}),
    V = wasm:run_js(
      <<"(a) => a.list[0].n + a.wrap.h.n">>,
      #{list => [H], wrap => #{h => H}}
    ),
    ok = wasm:send(#{nested => V}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("nested");
  expect(otp.events).toContainEqual({ nested: 10 });
});

test("final tracked value sends are delivered before cleanup", async ({
  otp,
  page,
}) => {
  const count = 8;
  const RUN_JS_BOOT_EVAL = trimLeft(`
    spawn(fun() ->
      lists:foreach(fun(I) ->
        H = wasm:run_js(
          <<"({i}) => new TrackedValue(
            {label: 'tracked-' + i},
            () => globalThis.popcorn.cleanup()
          )">>,
          #{i => I}
        ),
        ok = wasm:send(#{final_ref => H}),
        erlang:garbage_collect(self())
      end, lists:seq(1, ${count})),
      ok = wasm:send(#{final_ref_done => true})
    end).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("final_ref_done");

  const refs = valuesWithKey(otp.events, "final_ref");
  const labels = valuesWithKey(refs, "label");
  expect(labels).toEqual(
    Array.from({ length: count }, (_, i) => `tracked-${i + 1}`),
  );
  await expect.poll(() => getCleanups(page)).toBe(count);
});

test("tracked argument cleanup waits for async run_js to finish", async ({
  otp,
  page,
}) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    register(controller, self()),
    Runner = spawn(fun() ->
      receive
        {tracked, H} ->
          % Runner owns the final BEAM reference while JavaScript is suspended.
          V = wasm:run_js(
            <<"async ({h}) => {
              await globalThis.popcorn.runJs.pause();
              return {
                value: h.value,
                cleanups: globalThis.popcorn.cleanups,
              };
            }">>,
            #{h => H},
            [{timeout, 10000}]
          ),
          ok = wasm:send(#{async_tracked => V})
      end
    end),
    spawn(fun() ->
      H = wasm:run_js(
        <<"() => new TrackedValue(
          {value: 'tracked argument'},
          () => globalThis.popcorn.cleanup()
        )">>,
        #{}
      ),
      Runner ! {tracked, H},
      ok = wasm:send(#{async_runner_ready => true})
    end),
    receive
      {wasm, PayloadJson, _} ->
        #{<<"collect">> := true} = json:decode(PayloadJson),
        % Force collection while Runner is blocked inside wasm:run_js/3.
        erlang:garbage_collect(Runner),
        ok = wasm:send(#{async_runner_collected => true})
    end.
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("async_runner_ready");
  await waitForRunJsSuspension(page);

  const collect = await otp.send("controller", { collect: true });
  assert(collect.ok);
  await otp.waitForEvent("async_runner_collected");

  await expect
    .poll(() => getCleanups(page), {
      timeout: 500,
      intervals: [50],
    })
    .toBe(0);

  await finishRunJs(page);

  const event = await otp.waitForEvent("async_tracked");
  expect(event).toEqual({
    async_tracked: { value: "tracked argument", cleanups: 0 },
  });
  await expect.poll(() => getCleanups(page)).toBe(1);
});

test("isolated tracked values", async ({ createOtp }) => {
  const evalFor = (id: string) =>
    trimLeft(`
      H = wasm:run_js(<<"() => new TrackedValue({id: '${id}'})">>, #{}),
      ok = wasm:send(H).
    `);

  const [a, b] = await Promise.all([createOtp(), createOtp()]);
  const [aBoot, bBoot] = await Promise.all([
    a.boot(evalOpts(evalFor("A"))),
    b.boot(evalOpts(evalFor("B"))),
  ]);
  assert(aBoot.ok);
  assert(bBoot.ok);

  const aId = await a.waitForEvent("id");
  const bId = await b.waitForEvent("id");

  expect(aId).toEqual({ id: "A" });
  expect(bId).toEqual({ id: "B" });
});

test("tracked values isolated cleanup", async ({ createOtp, page }) => {
  const [closedOtp, liveOtp] = await Promise.all([createOtp(), createOtp()]);
  let cleanupCalls = 0;
  await page.exposeFunction("popcornCleanup", () => {
    cleanupCalls += 1;
  });
  await page.evaluate(() => {
    const scope = globalThis as unknown as {
      popcorn: { cleanup: () => void };
      popcornCleanup: () => void;
    };
    scope.popcorn = { cleanup: () => scope.popcornCleanup() };
  });
  const cleanupTrackedEval = trimLeft(`
    spawn(fun() ->
      H = wasm:run_js(
        <<"() => new TrackedValue(0, () => globalThis.popcorn.cleanup())">>,
        #{}
      ),
      ok = wasm:send(#{deinit_ready => true}),
      receive stop -> H end
    end).
  `);
  const liveEval = trimLeft(`
    register(controller, self()),
    HB = wasm:run_js(<<"() => new TrackedValue({v: 42})">>, #{}),
    ok = wasm:send(#{live_ready => true}),
    receive
      {wasm, _, _} ->
        ok = wasm:send(HB)
    end.
  `);

  const [closedBoot, liveBoot] = await Promise.all([
    closedOtp.boot(evalOpts(cleanupTrackedEval)),
    liveOtp.boot(evalOpts(liveEval)),
  ]);
  assert(closedBoot.ok);
  assert(liveBoot.ok);

  await closedOtp.waitForEvent("deinit_ready");
  await liveOtp.waitForEvent("live_ready");
  await closedOtp.dispose();
  await expect.poll(() => cleanupCalls).toBe(1);

  const send = await liveOtp.send("controller", { ping: true });
  assert(send.ok);
  const liveEvent = await liveOtp.waitForEvent("v");

  expect(liveEvent).toEqual({ v: 42 });
  expect(cleanupCalls).toBe(1);
});

test("send() to unregistered process", async ({ otp }) => {
  const READY_BOOT_EVAL = "ok = wasm:send(#{ready => true}).";
  const boot = await otp.boot(evalOpts(READY_BOOT_EVAL));
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

test("send() timeout", async ({ otp }) => {
  const boot = await otp.boot({
    beam: { manifestUrl: "/otp-assets/manifest.json" },
    timeoutsMs: { send: 0 },
  });
  assert(boot.ok);

  const send = await otp.send("any-target", { ping: true });
  expect(send).toEqual({
    ok: false,
    error: { t: "timeout:send", data: { timeoutMs: 0 } },
  });
});
