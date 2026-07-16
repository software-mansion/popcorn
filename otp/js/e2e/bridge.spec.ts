import { assert, evalOpts, expect, test, trimLeft } from "./helpers";
import { tuple2 } from "../src/etf";

const PAYLOAD = {
  text: "zażółć",
  nullValue: null,
  boolTrue: true,
  boolFalse: false,
  numbers: [
    0,
    2 ** 8 - 1,
    2 ** 8,
    -1,
    2 ** 31 - 1,
    -(2 ** 31),
    2 ** 31,
    -(2 ** 31) - 1,
    Number.MAX_SAFE_INTEGER,
    Number.MIN_SAFE_INTEGER,
    1.5,
  ],
  emptyList: [],
  emptyMap: {},
  nested: [{ value: "ok" }],
};

const META = { requestId: "req-1" };

test("ETF sorts map keys", () => {
  assert.equal(hex({ b: 2, a: 1 }), hex({ a: 1, b: 2 }));
});

test("ETF accepts null-prototype plain objects", () => {
  const value = Object.assign(Object.create(null), { key: "value" });
  assert(tuple2(value, {}).ok);
});

for (const [name, value, reason] of [
  ["unsafe integer", Number.MAX_SAFE_INTEGER + 1, "lossy-int"],
  ["positive infinity", Infinity, "non-finite-float"],
  ["negative infinity", -Infinity, "non-finite-float"],
  ["NaN", NaN, "non-finite-float"],
  ["undefined", undefined, "unsupported"],
  ["function", () => null, "unsupported"],
  ["symbol", Symbol("value"), "unsupported"],
  ["non-plain object", new Date(), "non-plain-object"],
] as const) {
  test(`ETF reports ${name}`, () => {
    const result = tuple2(value, {});
    assert(!result.ok);
    assert.equal(result.error.t, "bridge:unserializable");
    assert.equal(result.error.data.data, value);
    assert.equal(result.error.data.part, value);
    assert.equal(result.error.data.reason, reason);
  });
}

test("ETF reports cyclic values", () => {
  const value: unknown[] = [];
  value.push(value);
  const result = tuple2(value, {});
  assert(!result.ok);
  assert.equal(result.error.data.data, value);
  assert.equal(result.error.data.part, value);
  assert.equal(result.error.data.reason, "cyclic-object");
});

test("ETF encodes enumerable string properties", () => {
  const symbolKey = { visible: true, [Symbol("key")]: true };
  const hidden = Object.defineProperty({}, "hidden", { value: true });
  const accessor = Object.defineProperty({}, "value", {
    enumerable: true,
    get: () => true,
  });
  const extendedArray = Object.assign([], { extra: true });

  assert.equal(hex(symbolKey), hex({ visible: true }));
  assert.equal(hex(hidden), hex({}));
  assert.equal(hex(accessor), hex({ value: true }));
  assert.equal(hex(extendedArray), hex([]));
  const sparse = new Array(1);
  const result = tuple2(sparse, {});
  assert(!result.ok);
  assert.equal(result.error.data.data, sparse);
  assert.equal(result.error.data.part, undefined);
  assert.equal(result.error.data.reason, "unsupported");
});

test("ETF reports unsupported nested values", () => {
  const fn = () => null;
  const symbol = Symbol();
  for (const [value, part] of [
    [{ value: undefined }, undefined],
    [[fn], fn],
    [{ value: symbol }, symbol],
  ] as const) {
    const result = tuple2(value, {});
    assert(!result.ok);
    assert.equal(result.error.data.data, value);
    assert.equal(result.error.data.part, part);
    assert.equal(result.error.data.reason, "unsupported");
  }
});

test("ETF reports the failing metadata part with the full payload", () => {
  const data = { value: "ok" };
  const part = new Date();
  const result = tuple2(data, { part });
  assert(!result.ok);
  assert.equal(result.error.data.data, data);
  assert.equal(result.error.data.part, part);
  assert.equal(result.error.data.reason, "non-plain-object");
});

test("handles events in both directions", async ({ otp }) => {
  const BRIDGE_BOOT_EVAL = trimLeft(`
    spawn(fun() ->
      ExpectedPayload = #{
        <<"text">> => <<"zażółć"/utf8>>,
        <<"nullValue">> => nil,
        <<"boolTrue">> => true,
        <<"boolFalse">> => false,
        <<"numbers">> => [
          0,
          (1 bsl 8) - 1,
          1 bsl 8,
          -1,
          (1 bsl 31) - 1,
          -(1 bsl 31),
          1 bsl 31,
          -(1 bsl 31) - 1,
          (1 bsl 53) - 1,
          -((1 bsl 53) - 1),
          1.5
        ],
        <<"emptyList">> => [],
        <<"emptyMap">> => #{},
        <<"nested">> => [#{<<"value">> => <<"ok">>}]
      },
      ExpectedMeta = #{<<"requestId">> => <<"req-1">>},
      ExpectedEtf = base64:encode(term_to_binary({ExpectedPayload, ExpectedMeta})),
      ok = wasm:send(#{etf_expected => ExpectedEtf}),
      ok = wasm:send(#{direct => true, nested => #{count => 1}}),
      true = register(bridge, self()),
      Loop = fun(F) ->
        ok = wasm:send(#{bridge_ready => true}),
        receive
          {wasm, Payload, Meta} ->
            Shape = case {Payload, Meta} of
              {ExpectedPayload, ExpectedMeta} -> decoded;
              _ -> unexpected
            end,
            ok = wasm:send(#{reply => Payload, meta => Meta, shape => Shape})
        after 100 ->
            F(F)
        end
      end,
      Loop(Loop)
    end).
  `);
  const boot = await otp.boot(evalOpts(BRIDGE_BOOT_EVAL));

  assert(boot.ok);
  const expectedEtf = await otp.waitForEvent("etf_expected");
  expect(expectedEtf).toEqual({
    etf_expected: Buffer.from(encode(PAYLOAD, META)).toString("base64"),
  });
  await otp.waitForEvent("direct");
  expect(otp.events).toContainEqual({
    direct: true,
    nested: { count: 1 },
  });

  await otp.waitForEvent("bridge_ready");

  const send = await otp.send("bridge", structuredClone(PAYLOAD), {
    meta: structuredClone(META),
  });
  assert(send.ok);

  await otp.waitForEvent("reply");
  expect(otp.events).toContainEqual({
    reply: { ...PAYLOAD, nullValue: "nil" },
    meta: META,
    shape: "decoded",
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

function hex(payload: unknown, meta: unknown = {}): string {
  return Buffer.from(encode(payload, meta)).toString("hex");
}

function encode(payload: unknown, meta: unknown): Uint8Array<ArrayBuffer> {
  const result = tuple2(payload, meta);
  assert(result.ok);
  return result.data;
}
