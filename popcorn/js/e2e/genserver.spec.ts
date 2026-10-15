import { assert, evalOpts, expect, test } from "./helpers";

const STOPPABLE_GEN_SERVER = `
  {ok, Counter} = gen_server:start({local, counter}, test_gen_server, 0, []),
  Proxy = whereis(popcorn_proxy),
  [{'Elixir.Popcorn.Proxy', Proxy, worker, _}] =
    supervisor:which_children('Elixir.Popcorn.Supervisor'),
  true = unlink(Counter),
  ok = wasm:send(#{genserver_ready => true}),
  receive stop -> ok end.
`;

test.describe("genserver", () => {
  test("call and cast", async ({ otp }) => {
    const boot = await otp.tryBoot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");

    expect(await otp.tryCall("counter", ["add", 2])).toEqual({
      ok: true,
      data: 2,
    });
    expect(await otp.tryCast("counter", ["add", 4])).toEqual({
      ok: true,
      data: null,
    });
    expect(await otp.tryCall("counter", "get")).toEqual({
      ok: true,
      data: 6,
    });
  });

  test("values and serialization", async ({ otp, page }) => {
    const boot = await otp.tryBoot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");
    const request = await page.evaluateHandle(() => [
      "echo",
      {
        integer: 9007199254740993n,
        date: new Date(1704067200123),
      },
    ]);
    const result = await otp.tryCall("counter", request);
    assert(result.ok);
    const received = result.data as { integer: bigint; date: Date };
    expect(received.integer).toBe(9007199254740993n);
    expect(received.date.getTime()).toBe(1704067200123);
    await request.dispose();
    for (const request of [
      "unsupported",
      "bad_keys",
      "complex_key",
      "improper",
      "submillisecond",
    ]) {
      const reply = await otp.tryCall("counter", request);
      expect(reply).toEqual({
        ok: false,
        error: { t: "genserver:unserializable", data: {} },
      });
    }
    expect(await otp.tryCall("counter", "get")).toEqual({
      ok: true,
      data: 0,
    });
  });

  test("errors", async ({ otp }) => {
    const boot = await otp.tryBoot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");

    expect(
      await otp.tryCall("counter", "wait", { timeoutMs: 0 }),
    ).toEqual({
      ok: false,
      error: {
        t: "timeout:call",
        data: { timeoutMs: 0 },
      },
    });
    expect(await otp.tryCall("missing", "get")).toEqual({
      ok: false,
      error: {
        t: "genserver:noproc",
        data: { target: "missing" },
      },
    });
  });
});

test("throwing APIs", async ({ page }) => {
  const options = evalOpts(
    STOPPABLE_GEN_SERVER.replace(
      "ok = wasm:send(#{genserver_ready => true}),",
      `true = register(controller, self()),
     0 = wasm:run_js(<<"async (_, {call, cast, send}) => {
       await cast('counter', ['add', 0]);
       await send('controller', {});
       return await call('counter', 'get');
     }">>, #{}),
     ok = wasm:send(#{genserver_ready => true}),`,
    ),
  );
  const result = await page.evaluate(async (options) => {
    async function failure(operation: () => unknown) {
      try {
        await operation();
      } catch (error) {
        if (!(error instanceof Error)) throw error;
        return { name: error.name, cause: error.cause };
      }
      throw new Error("Expected failure");
    }

    let ready!: () => void;
    const readiness = new Promise<void>((resolve) => {
      ready = resolve;
    });
    const popcorn = await window.Popcorn.init({
      ...options,
      onEvent: () => ready(),
    });
    try {
      await readiness;
      const sameInstance = (await popcorn.boot()) === popcorn;
      const initial = await popcorn.call("counter", "get");
      const cast = await popcorn.cast("counter", ["add", 4]);
      const updated = await popcorn.call("counter", "get");
      const send = await popcorn.send("controller", {});
      const overflow = await failure(() =>
        popcorn.writeStdin(new Uint8Array(64 * 1024 + 1)),
      );
      const stdin = popcorn.writeStdin("x");
      const resize = popcorn.resizeTty(80, 24);
      const missing = await failure(() =>
        popcorn.call("missing", "get"),
      );
      const invalidInput = await failure(() => popcorn.writeStdin(""));
      const invalidOptions = await failure(() =>
        window.Popcorn.init({ tty: { size: { columns: 0, rows: 24 } } }),
      );
      popcorn.deinit();
      const closed = await failure(() => popcorn.send("controller", {}));
      const closedCast = await failure(() =>
        popcorn.cast("counter", "reset"),
      );
      const closedStdin = await failure(() => popcorn.writeStdin("x"));
      const closedResize = await failure(() => popcorn.resizeTty(80, 24));
      const timedOut = new window.Popcorn({
        workerUrl: "/fault-worker.mjs",
        timeoutsMs: { boot: 0 },
      });
      const bootFailure = await failure(() => timedOut.boot());
      timedOut.deinit();
      const initFailure = await failure(() =>
        window.Popcorn.init({
          workerUrl: "/fault-worker.mjs",
          timeoutsMs: { boot: 0 },
        }),
      );
      return {
        sameInstance,
        initial,
        updated,
        noValues: [cast, send, stdin, resize].every(
          (value) => value === undefined,
        ),
        missing,
        overflow,
        invalidInput,
        invalidOptions,
        closed,
        closedCast,
        closedStdin,
        closedResize,
        bootFailure,
        initFailure,
      };
    } finally {
      popcorn.deinit();
    }
  }, options);

  const closed = {
    name: "PopcornError",
    cause: { t: "vm:exited", data: { reason: "deinit" } },
  };
  const timeout = {
    name: "PopcornError",
    cause: { t: "timeout:init", data: { timeoutMs: 0 } },
  };
  expect(result).toEqual({
    sameInstance: true,
    initial: 0,
    updated: 4,
    noValues: true,
    missing: {
      name: "PopcornError",
      cause: { t: "genserver:noproc", data: { target: "missing" } },
    },
    overflow: {
      name: "PopcornError",
      cause: {
        t: "stdio:overflow",
        data: { capacityBytes: 65536, attemptedBytes: 65537 },
      },
    },
    invalidInput: {
      name: "PopcornError",
      cause: { t: "internal:check", data: { detail: undefined } },
    },
    invalidOptions: {
      name: "PopcornError",
      cause: { t: "internal:check", data: { detail: undefined } },
    },
    closed,
    closedCast: closed,
    closedStdin: closed,
    closedResize: closed,
    bootFailure: timeout,
    initFailure: timeout,
  });
});
