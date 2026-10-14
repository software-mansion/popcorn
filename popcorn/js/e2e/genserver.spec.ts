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
    const boot = await otp.boot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");

    expect(await otp.call("counter", ["add", 2])).toEqual({
      ok: true,
      data: 2,
    });
    expect(await otp.cast("counter", ["add", 4])).toEqual({
      ok: true,
      data: null,
    });
    expect(await otp.call("counter", "get")).toEqual({
      ok: true,
      data: 6,
    });
  });

  test("values and serialization", async ({ otp, page }) => {
    const boot = await otp.boot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");
    const request = await page.evaluateHandle(() => [
      "echo",
      {
        integer: 9007199254740993n,
        date: new Date(1704067200123),
      },
    ]);
    const result = await otp.call("counter", request);
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
      const reply = await otp.call("counter", request);
      expect(reply).toEqual({
        ok: false,
        error: { t: "genserver:unserializable", data: {} },
      });
    }
    expect(await otp.call("counter", "get")).toEqual({
      ok: true,
      data: 0,
    });
  });

  test("errors", async ({ otp }) => {
    const boot = await otp.boot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");

    expect(await otp.call("counter", "wait", { timeoutMs: 0 })).toEqual({
      ok: false,
      error: {
        t: "timeout:call",
        data: { timeoutMs: 0 },
      },
    });
    expect(await otp.call("missing", "get")).toEqual({
      ok: false,
      error: {
        t: "genserver:noproc",
        data: { target: "missing" },
      },
    });
  });
});
