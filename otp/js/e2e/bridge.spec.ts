import { encode } from "../src/etf";
import { assert, evalOpts, expect, test } from "./helpers";

const OVERFLOWED = Number.MAX_SAFE_INTEGER + 1;
const PAYLOAD = {
  text: "żółw",
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

test.describe("ETF", () => {
  test("objects", () => {
    const nullPrototype = Object.assign(Object.create(null), { key: "value" });
    const symbolKey = { visible: true, [Symbol("key")]: true };
    const hidden = Object.defineProperty({}, "hidden", { value: true });
    const accessor = Object.defineProperty({}, "value", {
      enumerable: true,
      get: () => true,
    });

    assert.equal(hex({ b: 2, a: 1 }), hex({ a: 1, b: 2 }));
    assert(encode(nullPrototype).ok);
    assert.equal(hex(symbolKey), hex({ visible: true }));
    assert.equal(hex(hidden), hex({}));
    assert.equal(hex(accessor), hex({ value: true }));
    assert.equal(hex(Object.assign([], { extra: true })), hex([]));
  });

  test("errors", () => {
    const cyclic: unknown[] = [];
    cyclic.push(cyclic);
    const fn = () => null;
    const symbol = Symbol("value");
    const date = new Date();
    const sparse = new Array(1);

    for (const [value, part, reason] of [
      [OVERFLOWED, OVERFLOWED, "lossy-int"],
      [Infinity, Infinity, "non-finite-float"],
      [-Infinity, -Infinity, "non-finite-float"],
      [NaN, NaN, "non-finite-float"],
      [cyclic, cyclic, "cyclic-object"],
      [undefined, undefined, "unsupported"],
      [fn, fn, "unsupported"],
      [symbol, symbol, "unsupported"],
      [date, date, "non-plain-object"],
      [{ value: undefined }, undefined, "unsupported"],
      [[fn], fn, "unsupported"],
      [sparse, undefined, "unsupported"],
    ] as const) {
      const result = encode(value);
      assert(!result.ok);
      assert.equal(result.error.data.data, value);
      assert.equal(result.error.data.part, part);
      assert.equal(result.error.data.reason, reason);
    }
  });
});

test.describe("events", () => {
  test("round trip", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
          spawn(fun() ->
            ExpectedPayload = #{
              <<"text">> => <<"żółw"/utf8>>,
              <<"nullValue">> => nil,
              <<"boolTrue">> => true,
              <<"boolFalse">> => false,
              <<"numbers">> => [
                0, 255, 256, -1, 2147483647, -2147483648,
                2147483648, -2147483649, 9007199254740991,
                -9007199254740991, 1.5
              ],
              <<"emptyList">> => [],
              <<"emptyMap">> => #{},
              <<"nested">> => [#{<<"value">> => <<"ok">>}]
            },
            ExpectedEtf = base64:encode(term_to_binary(ExpectedPayload)),
            ok = wasm:send(#{etf_expected => ExpectedEtf}),
            true = register('żółw', self()),
            ok = wasm:send(#{bridge_ready => true}),
            receive
              {wasm, Payload} ->
                ok = wasm:send(#{
                  reply => Payload,
                  decoded => Payload =:= ExpectedPayload
                })
            end
          end).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("etf_expected")).toEqual({
      etf_expected: Buffer.from(encodePayload(PAYLOAD)).toString("base64"),
    });
    await otp.waitForEvent("bridge_ready");
    assert((await otp.send("żółw", structuredClone(PAYLOAD))).ok);
    expect(await otp.waitForEvent("reply")).toEqual({
      reply: { ...PAYLOAD, nullValue: "nil" },
      decoded: true,
    });
  });
});

test.describe("run_js", () => {
  test("values", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
          Sync = wasm:run_js(<<"({a, b}) => a + b">>, #{a => 1, b => 2}),
          Async = wasm:run_js(
            <<"async ({a, b}) => a + b">>,
            #{a => 2, b => 5}
          ),
          Nested = wasm:run_js(
            <<"() => ({a: 1, nested: {b: [2, 3]}, flag: true})">>,
            #{},
            [{return, value}]
          ),
          ok = wasm:send(#{sync => Sync, async => Async, nested => Nested}).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("sync")).toEqual({
      sync: 3,
      async: 7,
      nested: { a: 1, nested: { b: [2, 3] }, flag: true },
    });
  });

  test("errors", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
          Timeout = try
            wasm:run_js(<<"() => new Promise(() => {})">>, #{}, [{timeout, 0}])
          catch
            error:run_js_timeout -> timeout
          end,
          Thrown = try
            wasm:run_js(<<"() => { throw new Error('boom') }">>, #{})
          catch
            error:{run_js, Message} -> Message
          end,
          Unserializable = try
            wasm:run_js(<<"() => () => 1">>, #{})
          catch
            error:{run_js, {unserializable, Reason}} -> Reason
          end,
          Invalid = try
            wasm:run_js(<<"() => 1">>, #{}, [{return, bogus}])
          catch
            error:function_clause -> invalid_option
          end,
          ok = wasm:send(#{
            timeout => Timeout,
            thrown => Thrown,
            unserializable => Unserializable,
            invalid => Invalid
          }).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("timeout")).toEqual({
      timeout: "timeout",
      thrown: "Error: boom",
      unserializable: "unsupported",
      invalid: "invalid_option",
    });
  });
});

test.describe("send", () => {
  test("errors", async ({ createOtp }) => {
    const otp = await createOtp();
    const boot = await otp.boot(
      evalOpts(`
        true = register(controller, self()),
        ok = wasm:send(#{ready => true}),
        receive _ -> ok end.
      `),
    );
    assert(boot.ok);
    await otp.waitForEvent("ready");

    expect(await otp.send("unknown", {})).toEqual({
      ok: false,
      error: {
        t: "bridge:listener-not-found",
        data: { targetName: "unknown" },
      },
    });
    expect(await otp.send("controller", OVERFLOWED)).toEqual({
      ok: false,
      error: {
        t: "bridge:unserializable",
        data: {
          data: OVERFLOWED,
          part: OVERFLOWED,
          reason: "lossy-int",
        },
      },
    });

    const timedOut = await createOtp();
    const bootOpts = {
      beam: { manifestUrl: "/assets/otp/manifest.json" },
      timeoutsMs: { send: 0 },
    };
    assert((await timedOut.boot(bootOpts)).ok);
    expect(await timedOut.send("any-target", {})).toEqual({
      ok: false,
      error: { t: "timeout:send", data: { timeoutMs: 0 } },
    });
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
