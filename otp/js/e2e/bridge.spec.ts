import { assert, evalOpts, expect, test, trimLeft } from "./helpers";
import { encode } from "../src/etf";

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

test("ETF sorts map keys", () => {
  assert.equal(hex({ b: 2, a: 1 }), hex({ a: 1, b: 2 }));
});

test("ETF accepts null-prototype plain objects", () => {
  const value = Object.assign(Object.create(null), { key: "value" });
  assert(encode(value).ok);
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
    const result = encode(value);
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
  const result = encode(value);
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
  const result = encode(sparse);
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
    const result = encode(value);
    assert(!result.ok);
    assert.equal(result.error.data.data, value);
    assert.equal(result.error.data.part, part);
    assert.equal(result.error.data.reason, "unsupported");
  }
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
      ExpectedEtf = base64:encode(term_to_binary(ExpectedPayload)),
      ok = wasm:send(#{etf_expected => ExpectedEtf}),
      ok = wasm:send(#{direct => true, nested => #{count => 1}}),
      true = register(bridge, self()),
      Loop = fun(F) ->
        ok = wasm:send(#{bridge_ready => true}),
        receive
          {wasm, Payload} ->
            Shape = case Payload of
              ExpectedPayload -> decoded;
              _ -> unexpected
            end,
            ok = wasm:send(#{reply => Payload, shape => Shape})
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
    etf_expected: Buffer.from(encodePayload(PAYLOAD)).toString("base64"),
  });
  await otp.waitForEvent("direct");
  expect(otp.events).toContainEqual({
    direct: true,
    nested: { count: 1 },
  });

  await otp.waitForEvent("bridge_ready");

  const send = await otp.send("bridge", structuredClone(PAYLOAD));
  assert(send.ok);

  await otp.waitForEvent("reply");
  expect(otp.events).toContainEqual({
    reply: { ...PAYLOAD, nullValue: "nil" },
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

test("run_js: return nested value", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    V = wasm:run_js(
      <<"() => ({a: 1, s: 'x', nested: {b: [2, 3]}, flag: true})">>,
      #{},
      [{return, value}]
    ),
    ok = wasm:send(#{value_mode => V}).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("value_mode");
  expect(otp.events).toContainEqual({
    value_mode: { a: 1, s: "x", nested: { b: [2, 3] }, flag: true },
  });
});

test("run_js: throws when returning unserializable value", async ({ otp }) => {
  // One case per codec reason (unsupported/non_finite_float/lossy_int/
  // non_plain_object/cyclic_object).
  const REJECTIONS = [
    ["func", "() => () => 1", "unsupported"],
    ["symbol", "() => Symbol('x')", "unsupported"],
    ["bigint", "() => 1n", "unsupported"],
    ["pos_inf", "() => Infinity", "non_finite_float"],
    ["neg_inf", "() => -Infinity", "non_finite_float"],
    ["nan", "() => NaN", "non_finite_float"],
    ["unsafe_int", "() => Number.MAX_SAFE_INTEGER + 1", "lossy_int"],
    ["non_plain", "() => new Date()", "non_plain_object"],
    ["cyclic", "() => { const a = []; a.push(a); return a; }", "cyclic_object"],
  ] as const;
  const cases = REJECTIONS.map(
    ([name, code]) => `{${name}, <<"${code}">>}`,
  ).join(",\n      ");
  const RUN_JS_BOOT_EVAL = trimLeft(`
    Cases = [
      ${cases}
    ],
    lists:foreach(fun({Name, Code}) ->
      Result = try wasm:run_js(Code, #{}) of
        _ -> #{outcome => no_error}
      catch
        error:{run_js, {unserializable, Reason}} ->
          #{outcome => rejected, reason => Reason};
        error:run_js_timeout -> #{outcome => timeout}
      end,
      ok = wasm:send(#{reject => Name, result => Result})
    end, Cases).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  for (const [name, , reason] of REJECTIONS) {
    await otp.waitForEvent("reject");
    expect(otp.events).toContainEqual({
      reject: name,
      result: { outcome: "rejected", reason },
    });
  }
});

test("run_js: options validation", async ({ otp }) => {
  const RUN_JS_BOOT_EVAL = trimLeft(`
    Value = wasm:run_js(<<"() => 1">>, #{}, [{return, value}]),
    Ref = wasm:run_js(<<"() => 1">>, #{}, [{return, ref}]),
    Invalid = try wasm:run_js(<<"() => 1">>, #{}, [{return, bogus}]) of
      _ -> ok
    catch
      error:function_clause -> invalid_option
    end,
    ok = wasm:send(#{
      value_ok => Value =:= 1,
      ref_ok => is_tuple(Ref) andalso element(1, Ref) =:= wasm_tracked_value,
      invalid_return => Invalid
    }).
  `);
  const boot = await otp.boot(evalOpts(RUN_JS_BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("invalid_return");
  expect(otp.events).toContainEqual({
    value_ok: true,
    ref_ok: true,
    invalid_return: "invalid_option",
  });
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

function hex(payload: unknown): string {
  return Buffer.from(encodePayload(payload)).toString("hex");
}

function encodePayload(payload: unknown): Uint8Array<ArrayBuffer> {
  const result = encode(payload);
  assert(result.ok);
  return result.data;
}
