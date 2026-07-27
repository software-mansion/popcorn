import type { Pid } from "@swmansion/popcorn-otp";
import { assert, evalOpts, expect, test } from "./helpers";

test.describe("pid", () => {
  test("delayed send", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
          true = register(controller, self()),
          wasm:run_js(
            <<"(args, {send}) => {
              setTimeout(() => send('controller', {delayed: true}), 10);
              return null;
            }">>,
            #{}
          ),
          receive
            {wasm, #{<<"delayed">> := true}} ->
              ok = wasm:send(#{named_send => true})
          end.
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("named_send")).toEqual({ named_send: true });
  });

  test("round trip", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
          Self = self(),
          Self = wasm:run_js(<<"({target}) => target">>, #{target => Self}),
          #{<<"nested">> := #{<<"pid">> := Self}, <<"list">> := [Self]} =
            wasm:run_js(
              <<"({target}) => ({nested: {pid: target}, list: [target]})">>,
              #{target => Self}
            ),
          wasm:run_js(
            <<"(args, {send}) => {
              send(args.target, {forwarded: args.target});
              return null;
            }">>,
            #{target => Self}
          ),
          receive
            {wasm, #{<<"forwarded">> := Self}} ->
              ok = wasm:send(#{pid_roundtrip => true})
          end.
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("pid_roundtrip")).toEqual({
      pid_roundtrip: true,
    });
  });

  test("invalid", async ({ createOtp, otp }) => {
    const deadBoot = await otp.boot(
      evalOpts(`
          Dead = spawn(fun() -> ok end),
          Ref = erlang:monitor(process, Dead),
          receive {'DOWN', Ref, process, Dead, _} -> ok end,
          Ok = wasm:run_js(
            <<"(args, {send}) => send(args.target, {}).then(result => result.ok)">>,
            #{target => Dead}
          ),
          ok = wasm:send(#{dead => Ok}).
        `),
    );
    assert(deadBoot.ok);
    expect(await otp.waitForEvent("dead")).toEqual({ dead: false });

    const pidOpts = evalOpts(`
      ok = wasm:send(#{pid => self()}),
      receive _ -> ok end.
    `);
    const [owner, foreign] = await Promise.all([createOtp(), createOtp()]);
    const [ownerBoot, foreignBoot] = await Promise.all([
      owner.boot(pidOpts),
      foreign.boot(pidOpts),
    ]);
    assert(ownerBoot.ok);
    assert(foreignBoot.ok);
    const pid = await owner.eventValueHandle<Pid>("pid");

    assert((await owner.send(pid, {})).ok);
    expect(await foreign.send(pid, {})).toEqual({
      ok: false,
      error: { t: "bridge:invalid-target", data: {} },
    });

    await owner.deinit();
    assert((await owner.boot(pidOpts)).ok);
    expect(await owner.send(pid, {})).toEqual({
      ok: false,
      error: { t: "bridge:invalid-target", data: {} },
    });
  });
});
