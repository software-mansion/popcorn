import { assert, evalOpts, expect, test, trimLeft } from "./helpers";

test("injected send delivers to a named process (sync)", async ({ otp }) => {
  const BOOT_EVAL = trimLeft(`
    true = register(controller, self()),
    wasm:run_js(
      <<"(args, send) => { send('controller', {from_js: true}); return null; }">>,
      #{}
    ),
    receive
      {wasm, Payload, _} ->
        #{<<"from_js">> := true} = Payload,
        ok = wasm:send(#{named_send_ok => true})
    end.
  `);
  const boot = await otp.boot(evalOpts(BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("named_send_ok");
  expect(otp.events).toContainEqual({ named_send_ok: true });
});

test("injected send works from a later timer callback", async ({ otp }) => {
  const BOOT_EVAL = trimLeft(`
    true = register(controller, self()),
    wasm:run_js(
      <<"(args, send) => { setTimeout(() => send('controller', {delayed: true}), 10); return null; }">>,
      #{}
    ),
    receive
      {wasm, Payload, _} ->
        #{<<"delayed">> := true} = Payload,
        ok = wasm:send(#{delayed_send_ok => true})
    end.
  `);
  const boot = await otp.boot(evalOpts(BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("delayed_send_ok");
  expect(otp.events).toContainEqual({ delayed_send_ok: true });
});

test("injected send addresses a pid passed via args", async ({ otp }) => {
  const BOOT_EVAL = trimLeft(`
    Self = self(),
    wasm:run_js(
      <<"(args, send) => { send(args.target, {via_pid: true}); return null; }">>,
      #{target => Self}
    ),
    receive
      {wasm, Payload, _} ->
        #{<<"via_pid">> := true} = Payload,
        ok = wasm:send(#{pid_send_ok => true})
    end.
  `);
  const boot = await otp.boot(evalOpts(BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("pid_send_ok");
  expect(otp.events).toContainEqual({ pid_send_ok: true });
});

test("pid round-trips through run_js args and result", async ({ otp }) => {
  const BOOT_EVAL = trimLeft(`
    Self = self(),
    Self = wasm:run_js(<<"(args) => args.target">>, #{target => Self}),
    #{<<"nested">> := #{<<"p">> := Self}, <<"list">> := [Self]} =
      wasm:run_js(
        <<"(args) => ({nested: {p: args.target}, list: [args.target]})">>,
        #{target => Self}
      ),
    ok = wasm:send(#{pid_roundtrip_ok => true}).
  `);
  const boot = await otp.boot(evalOpts(BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("pid_roundtrip_ok");
  expect(otp.events).toContainEqual({ pid_roundtrip_ok: true });
});

test("pid forwarded inside a send payload revives to a real pid", async ({
  otp,
}) => {
  const BOOT_EVAL = trimLeft(`
    Parent = self(),
    Receiver = spawn(fun() ->
      receive
        {wasm, Payload, _} ->
          #{<<"forwarded">> := Pid} = Payload,
          Parent ! {forwarded_pid, Pid}
      end
    end),
    true = register(pidreceiver, Receiver),
    wasm:run_js(
      <<"(args, send) => { send('pidreceiver', {forwarded: args.target}); return null; }">>,
      #{target => Parent}
    ),
    receive
      {forwarded_pid, Got} ->
        ok = wasm:send(#{payload_pid_ok => Got =:= Parent})
    end.
  `);
  const boot = await otp.boot(evalOpts(BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("payload_pid_ok");
  expect(otp.events).toContainEqual({ payload_pid_ok: true });
});

test("sending to a dead process fails without crashing", async ({ otp }) => {
  const BOOT_EVAL = trimLeft(`
    Dead = spawn(fun() -> ok end),
    Ref = erlang:monitor(process, Dead),
    receive {'DOWN', Ref, process, Dead, _} -> ok end,
    Ok = wasm:run_js(
      <<"(args, send) => send(args.target, {ping: true}).then(r => r.ok)">>,
      #{target => Dead}
    ),
    ok = wasm:send(#{dead_send_ok => Ok}).
  `);
  const boot = await otp.boot(evalOpts(BOOT_EVAL));
  assert(boot.ok);

  await otp.waitForEvent("dead_send_ok");
  expect(otp.events).toContainEqual({ dead_send_ok: false });
});

test("a pid is scoped to the instance that minted it", async ({ createOtp }) => {
  const [a, b] = await Promise.all([createOtp(), createOtp()]);
  const [aPid] = await Promise.all([
    a.captureOwnPid(),
    b.boot(evalOpts("ok = wasm:send(#{ready => true}).")),
  ]);

  // The minting instance recognizes its own pid and delivers to it.
  const own = await a.sendToPid(aPid);
  expect(own.ok).toBe(true);

  const foreign = await b.sendToPid(aPid);
  expect(foreign).toEqual({
    ok: false,
    error: { t: "bridge:invalid-target", data: {} },
  });
});

test("a pid is invalid after its instance reboots", async ({ createOtp }) => {
  const otp = await createOtp();
  const stalePid = await otp.captureOwnPid();

  const reboot = await otp.reboot();
  expect(reboot.ok).toBe(true);

  const stale = await otp.sendToPid(stalePid);
  expect(stale).toEqual({
    ok: false,
    error: { t: "bridge:invalid-target", data: {} },
  });
});
