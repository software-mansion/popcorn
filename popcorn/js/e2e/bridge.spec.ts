import type { Pid } from "@swmansion/popcorn";
import { a, atom, encode, t, tuple } from "../src/etf";
import { assert, evalOpts, expect, test } from "./helpers";

const LARGE_INTEGER = -(1n << 2048n);
const OVERFLOWED = Number.MAX_SAFE_INTEGER + 1;
const PAYLOAD = {
  text: "żółw",
  nullValue: null,
  nilString: "nil",
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
  nested: [{ value: "ok", missing: null }, null],
};

test.describe("ETF", () => {
  test("nil", () => {
    assert.equal(hex(null), hex(atom("nil")));
    assert.equal(hex(undefined), hex(null));
    assert.equal(
      hex({ value: undefined, nested: [undefined, tuple(undefined, null)] }),
      hex({ value: null, nested: [null, tuple(null, null)] }),
    );
    assert.equal(hex(new Array(1)), hex([null]));
  });

  test("atoms and tuples", () => {
    const tupleJs = tuple as (...entries: unknown[]) => unknown;

    assert.equal(hex(atom("ok")), hex(a("ok")));
    assert.equal(hex(tuple(atom("ok"), "value")), hex(t(a("ok"), "value")));
    assert.throws(() => tupleJs("only"), /at least two entries/);
  });

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

  test("bytes and integers", () => {
    const storage = new Uint8Array([1, 0, 128, 255, 2]);
    const bytes = storage.subarray(1, 4);
    const view = new DataView(storage.buffer, 1, 3);
    const buffer = Buffer.from([0, 128, 255]);
    const floats = new Float32Array([1.5]);
    const floatBytes = new Uint8Array(floats.buffer);
    const largeInteger = encodePayload(LARGE_INTEGER);
    const safeInteger = Number.MAX_SAFE_INTEGER;
    const safeBigInt = BigInt(safeInteger);

    assert.equal(hex(bytes), "836d000000030080ff");
    assert.equal(hex(view), hex(bytes));
    assert.equal(hex(buffer), hex(bytes));
    assert.equal(hex(floats), hex(floatBytes));
    assert.equal(hex(new ArrayBuffer(0)), hex(""));
    assert.equal(hex(42n), hex(42));
    assert.equal(hex(safeBigInt), hex(safeInteger));
    assert.equal(largeInteger[1], 0x6f);
  });

  test("containers", () => {
    const map = new Map<string, number>([
      ["b", 2],
      ["a", 1],
    ]);
    const set = new Set([1, 1n]);
    const normalizedSet = new Set([1]);
    const minDate = new Date("-009999-01-01T00:00:00.000Z");
    const maxDate = new Date("9999-12-31T23:59:59.999Z");
    const outsideRange = new Date("+010000-01-01T00:00:00.000Z");
    const numericKeys = new Map<unknown, boolean>([
      [1, true],
      [1n, false],
    ]);
    const objectKey = { a: 1, b: 2 };
    const equalKeys = new Map<unknown, boolean>([
      [map, true],
      [objectKey, false],
    ]);
    const cyclicMap = new Map();
    cyclicMap.set("self", cyclicMap);
    const cyclicSet = new Set();
    cyclicSet.add(cyclicSet);

    assert.equal(hex(map), hex({ a: 1, b: 2 }));
    assert.equal(hex(set), hex(normalizedSet));
    assert.equal(hex(Symbol.for("ok")), hex(atom("ok")));
    assert(encode(minDate).ok);
    assert(encode(maxDate).ok);
    assert(!encode(outsideRange).ok);
    assert(!encode(atom("a".repeat(256))).ok);
    assert(!encode(numericKeys).ok);
    assert(!encode(equalKeys).ok);
    assert(!encode(cyclicMap).ok);
    assert(!encode(cyclicSet).ok);
  });

  test("errors", () => {
    const cyclic: unknown[] = [];
    cyclic.push(cyclic);
    const fn = () => null;
    const symbol = Symbol();
    const date = new Date(NaN);

    for (const [value, part, reason] of [
      [OVERFLOWED, OVERFLOWED, "lossy-int"],
      [Infinity, Infinity, "non-finite-float"],
      [-Infinity, -Infinity, "non-finite-float"],
      [NaN, NaN, "non-finite-float"],
      [cyclic, cyclic, "cyclic-object"],
      [fn, fn, "unsupported"],
      [symbol, symbol, "unsupported"],
      [date, date, "unsupported"],
      [[fn], fn, "unsupported"],
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
  test("large payloads", async ({ otp, page }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
        true = register(term_receiver, self()),
        ok = wasm:send(#{receiver => self()}),
        receive
          {wasm, Payload} ->
            Text = maps:get(<<"blob">>, Payload),
            ok = wasm:send(#{text_valid => Text =:= binary:copy(<<"A">>, 100000)})
        end,
        receive
          {wasm, Binary} ->
            ok = wasm:send(#{bytes_valid => Binary =:= binary:copy(<<165>>, 1048576)})
        end,
        receive
          {wasm, <<"ping">>} -> ok = wasm:send(#{alive => true})
        end.
      `),
    );
    assert(boot.ok);
    const receiver = await otp.eventValueHandle<Pid>("receiver");
    const payload = { blob: "A".repeat(100_000) };
    expect(await otp.trySend("no_such_name", payload)).toEqual({
      ok: false,
      error: {
        t: "bridge:listener-not-found",
        data: { targetName: "no_such_name" },
      },
    });
    expect(await otp.trySend("term_receiver", payload)).toEqual({
      ok: true,
      data: null,
    });
    expect(await otp.waitForEvent("text_valid")).toEqual({ text_valid: true });

    const bytes = await page.evaluateHandle(() =>
      new Uint8Array(1024 * 1024).fill(0xa5),
    );
    expect(await otp.trySend(receiver, bytes)).toEqual({ ok: true, data: null });
    await bytes.dispose();
    expect(await otp.waitForEvent("bytes_valid")).toEqual({ bytes_valid: true });
    expect(await otp.trySend(receiver, "ping")).toEqual({ ok: true, data: null });
    expect(await otp.waitForEvent("alive")).toEqual({ alive: true });
    await receiver.dispose();
  });

  test("bytes and integers", async ({ otp, page }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
      true = register(term_receiver, self()),
      ok = wasm:send(#{bridge_ready => true}),
      receive
        {wasm, [Binary, Integer, Small]} ->
          true = is_binary(Binary),
          3 = byte_size(Binary),
          true = is_integer(Integer),
          42 = Small,
          ok = wasm:send(#{received => [Binary, Integer, Small]})
      end,
      Args = #{bytes => <<0, 128, 255>>, text => <<"żółw"/utf8>>, big => 9007199254740993},
      #{<<"bytes">> := <<0, 128, 255>>, <<"text">> := <<"żółw"/utf8>>,
        <<"big">> := 9007199254740993} = wasm:run_js(<<"args => args">>, Args),
      ok = wasm:send(#{run_js_bytes => true}),
      receive stop -> ok end.
    `),
    );
    assert(boot.ok);
    await otp.waitForEvent("bridge_ready");
    const payload = await page.evaluateHandle((integer) => {
      const storage = new Uint8Array([1, 0, 128, 255, 2]);
      const bytes = new DataView(storage.buffer, 1, 3);
      return [bytes, integer, 42n];
    }, LARGE_INTEGER);
    const sent = await otp.trySend("term_receiver", payload);
    assert(sent.ok);
    await payload.dispose();

    const handle =
      await otp.eventValueHandle<[Uint8Array, bigint, number]>("received");
    const received = await handle.evaluate(([bytes, integer, small]) => ({
      isBytes: bytes instanceof Uint8Array,
      bytes: Array.from(bytes),
      integer,
      small,
    }));
    await handle.dispose();
    expect(received).toEqual({
      isBytes: true,
      bytes: [0, 128, 255],
      integer: LARGE_INTEGER,
      small: 42,
    });
    expect(await otp.waitForEvent("run_js_bytes")).toEqual({
      run_js_bytes: true,
    });
  });

  test("native containers", async ({ otp, page }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
      true = register(term_receiver, self()),
      ok = wasm:send(#{bridge_ready => true}),
      receive
        {wasm, [DateTime, Set, Map, ok]} ->
          1704067200123 = 'Elixir.DateTime':to_unix(DateTime, millisecond),
          2 = 'Elixir.MapSet':size(Set),
          true = 'Elixir.MapSet':'member?'(Set, 1),
          ExtendedSet = 'Elixir.MapSet':put(Set, 2),
          3 = 'Elixir.MapSet':size(ExtendedSet),
          #{<<"key">> := 42} = Map,
          Returned = wasm:run_js(<<"args => args">>, #{date => DateTime, set => Set}),
          #{<<"date">> := DateTime, <<"set">> := Set} = Returned,
          ok = wasm:send(#{containers => true})
      end,
      ok = wasm:send(#{escaped => #{
        pid => #{popcorn_pid => <<"ordinary">>},
        ref => #{popcorn_ref => 999},
        value => #{popcorn_value => [<<"integer">>, <<"999">>]},
        proto => #{<<"__proto__">> => #{safe => true}},
        fields => #{<<"constructor">> => <<"own">>, nested => <<128>>}
      }}),
      receive stop -> ok end.
    `),
    );
    assert(boot.ok);
    await otp.waitForEvent("bridge_ready");
    const payload = await page.evaluateHandle(() => {
      const date = new Date(1704067200123);
      const set = new Set([1, 1n, "text"]);
      const map = new Map([["key", 42]]);
      return [date, set, map, Symbol.for("ok")];
    });
    const sent = await otp.trySend("term_receiver", payload);
    assert(sent.ok);
    await payload.dispose();
    expect(await otp.waitForEvent("containers")).toEqual({ containers: true });

    const handle =
      await otp.eventValueHandle<Record<string, Record<string, unknown>>>(
        "escaped",
      );
    const escaped = await handle.evaluate((values) => ({
      pid: values.pid,
      ref: values.ref,
      value: values.value,
      ownProto: Object.hasOwn(values.proto, "__proto__"),
      plainProto: Object.getPrototypeOf(values.proto) === Object.prototype,
      protoValue: values.proto.__proto__,
      constructor: values.fields["constructor"],
      bytes: Array.from(values.fields.nested as Uint8Array),
    }));
    await handle.dispose();
    expect(escaped).toEqual({
      pid: { popcorn_pid: "ordinary" },
      ref: { popcorn_ref: 999 },
      value: { popcorn_value: ["integer", "999"] },
      ownProto: true,
      plainProto: true,
      protoValue: { safe: true },
      constructor: "own",
      bytes: [128],
    });
    const unknown = await page.evaluateHandle(() =>
      Symbol("popcorn_unknown_symbol_atom"),
    );
    const result = await otp.trySend("term_receiver", unknown);
    assert(!result.ok);
    assert.equal(result.error.t, "bridge:unserializable");
    await unknown.dispose();
  });

  test("local fun ETF round trip", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
          Fun = fun(Value) -> Value + 1 end,
          Encoded = term_to_binary(Fun),
          Decoded = binary_to_term(Encoded),
          {xxh3_128, Checksum} = lists:module_info(checksum),
          {ok, {lists, {xxh3_128, Checksum}}} =
            beam_lib:checksum(code:which(lists)),
          undefined = lists:module_info(md5),
          Info = lists:module_info(),
          {checksum, {xxh3_128, Checksum}} =
            lists:keyfind(checksum, 1, Info),
          {md5, undefined} = lists:keyfind(md5, 1, Info),
          ok = wasm:send(#{
            local_fun_round_trip => Decoded(1) =:= 2,
            new_fun_ext => binary:at(Encoded, 1) =:= 112,
            module_checksum => byte_size(Checksum)
          }).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("local_fun_round_trip")).toEqual({
      local_fun_round_trip: true,
      new_fun_ext: true,
      module_checksum: 16,
    });
  });

  test("atoms and tuples", async ({ otp, page }) => {
    function inJs<Result>(fn: () => Result) {
      return page.evaluateHandle(fn);
    }

    const boot = await otp.tryBoot(
      evalOpts(`
          true = register(term_receiver, self()),
          ok = wasm:send(#{bridge_ready => true}),
          receive
            {wasm, Payload} ->
              ok = wasm:send(#{
                term_received => Payload =:= {ok, <<"value">>}
              })
          end,
          receive
            stop -> ok
          end.
        `),
    );
    assert(boot.ok);

    await otp.waitForEvent("bridge_ready");
    const tuplePayload = await inJs(() => {
      const a = window.popcorn.atom;
      const t = window.popcorn.tuple;
      return t(a("ok"), "value");
    });
    assert((await otp.trySend("term_receiver", tuplePayload)).ok);
    await tuplePayload.dispose();
    expect(await otp.waitForEvent("term_received")).toEqual({
      term_received: true,
    });

    const atomPayload = await inJs(() => {
      const a = window.popcorn.atom;
      return a("popcorn_atom_that_does_not_exist");
    });
    const result = await otp.trySend("term_receiver", atomPayload);
    await atomPayload.dispose();
    assert(!result.ok);
    assert.equal(result.error.t, "bridge:unserializable");
  });

  test("round trip", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
          spawn(fun() ->
            ExpectedPayload = #{
              <<"text">> => <<"żółw"/utf8>>,
              <<"nullValue">> => nil,
              <<"nilString">> => <<"nil">>,
              <<"boolTrue">> => true,
              <<"boolFalse">> => false,
              <<"numbers">> => [
                0, 255, 256, -1, 2147483647, -2147483648,
                2147483648, -2147483649, 9007199254740991,
                -9007199254740991, 1.5
              ],
              <<"emptyList">> => [],
              <<"emptyMap">> => #{},
              <<"nested">> => [#{<<"value">> => <<"ok">>, <<"missing">> => nil}, nil]
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
    assert((await otp.trySend("żółw", structuredClone(PAYLOAD))).ok);
    expect(await otp.waitForEvent("reply")).toEqual({
      reply: PAYLOAD,
      decoded: true,
    });
  });
});

test.describe("run_js", () => {
  test("values", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
          Sync = wasm:run_js(<<"({a, b}) => a + b">>, #{a => 1, b => 2}),
          Async = wasm:run_js(
            <<"async ({a, b}) => a + b">>,
            #{a => 2, b => 5}
          ),
          Nested = wasm:run_js(
            <<"({missing}) => ({a: 1, nested: {b: [2, 3]}, flag: missing === null, missing})">>,
            #{missing => nil},
            [{return, value}]
          ),
          ok = wasm:send(#{sync => Sync, async => Async, nested => Nested}).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("sync")).toEqual({
      sync: 3,
      async: 7,
      nested: { a: 1, nested: { b: [2, 3] }, flag: true, missing: null },
    });
  });

  test("errors", async ({ otp, page }) => {
    const browserError = page.waitForEvent("pageerror");
    const boot = await otp.tryBoot(
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
          Assertion = try
            wasm:run_js(<<"() => { throw 'boom' }">>, #{}, [{timeout, infinity}])
          catch
            error:{run_js, AssertionMessage} -> AssertionMessage
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
            assertion => Assertion,
            unserializable => Unserializable,
            invalid => Invalid
          }).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("timeout")).toEqual({
      timeout: "timeout",
      thrown: "Error: boom",
      assertion:
        "Error: JS.run/3 must throw an Error instance. Received: boom",
      unserializable: "unsupported",
      invalid: "invalid_option",
    });
    expect((await browserError).message).toBe(
      "JS.run/3 must throw an Error instance. Received: boom",
    );
  });
});

test.describe("send", () => {
  test("errors", async ({ createOtp }) => {
    const otp = await createOtp();
    const boot = await otp.tryBoot(
      evalOpts(`
        true = register(controller, self()),
        ok = wasm:send(#{ready => true}),
        receive _ -> ok end.
      `),
    );
    assert(boot.ok);
    await otp.waitForEvent("ready");

    expect(await otp.trySend("unknown", {})).toEqual({
      ok: false,
      error: {
        t: "bridge:listener-not-found",
        data: { targetName: "unknown" },
      },
    });
    expect(await otp.trySend("controller", OVERFLOWED)).toEqual({
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
      timeoutsMs: { send: 0 },
    };
    assert((await timedOut.tryBoot(bootOpts)).ok);
    expect(await timedOut.trySend("any-target", {})).toEqual({
      ok: false,
      error: { t: "timeout", data: { operation: "send", timeoutMs: 0 } },
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
