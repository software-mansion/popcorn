import { assert, evalOpts, expect, test } from "./helpers";

const STOPPABLE_GEN_SERVER = `
  {ok, Counter} = gen_server:start({local, counter}, test_gen_server, 0, []),
  {ok, Proxy} = 'Elixir.Popcorn.Proxy':start_link([]),
  true = unlink(Counter),
  true = unlink(Proxy),
  ok = wasm:send(#{genserver_ready => true}),
  receive stop -> ok end.
`;

test.describe("genserver", () => {
  test("call and cast", async ({ otp }) => {
    const boot = await otp.boot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");

    expect(await otp.genserver.call("counter", ["add", 2])).toEqual({
      ok: true,
      data: 2,
    });
    expect(await otp.genserver.cast("counter", ["add", 4])).toEqual({
      ok: true,
      data: null,
    });
    expect(await otp.genserver.call("counter", "get")).toEqual({
      ok: true,
      data: 6,
    });
  });

  test("errors", async ({ otp }) => {
    const boot = await otp.boot(evalOpts(STOPPABLE_GEN_SERVER));
    assert(boot.ok);
    await otp.waitForEvent("genserver_ready");

    expect(
      await otp.genserver.call("counter", "wait", { timeoutMs: 0 }),
    ).toEqual({
      ok: false,
      error: {
        t: "timeout:call",
        data: { timeoutMs: 0 },
      },
    });
    expect(await otp.genserver.call("missing", "get")).toEqual({
      ok: false,
      error: {
        t: "genserver:noproc",
        data: { target: "missing" },
      },
    });
  });
});
