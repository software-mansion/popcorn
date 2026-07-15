import {
  assert,
  evalOpts,
  expect,
  finishRunJs,
  getCleanups,
  test,
  trimLeft,
  valuesWithKey,
  waitForRunJsSuspension,
} from "./helpers";

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
      {wasm, #{<<"collect">> := true}, _} ->
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

