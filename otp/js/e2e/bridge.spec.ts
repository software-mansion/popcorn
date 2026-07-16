import { assert, evalOpts, expect, test, trimLeft } from "./helpers";

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
    beam: { manifestUrl: "/assets/otp/manifest.json" },
    timeoutsMs: { send: 0 },
  });
  assert(boot.ok);

  const send = await otp.send("any-target", { ping: true });
  expect(send).toEqual({
    ok: false,
    error: { t: "timeout:send", data: { timeoutMs: 0 } },
  });
});

