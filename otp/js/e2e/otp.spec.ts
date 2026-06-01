import { assert, expect, test } from "./helpers";

const READY_BOOT_EVAL = "ok = wasm:send(#{ready => true}).";

test("boots", async ({ otp }) => {
  const result = await otp.boot({ beam: { assetsUrl: "/otp-assets" } });

  expect(result).toEqual({ ok: true, data: null });
});

test("reports timeouts on init", async ({ otp }) => {
  const result = await otp.boot({
    beam: { assetsUrl: "/otp-assets" },
    timeoutsMs: { boot: 0 },
  });

  expect(result).toEqual({
    ok: false,
    error: { t: "timeout:init", data: { timeoutMs: 0 } },
  });
});

test("handles missing boot script", async ({ otp }) => {
  const result = await otp.boot({
    beam: { assetsUrl: "/otp-assets/missing" },
  });

  expect(result).toEqual({
    ok: false,
    error: {
      t: "beam:missing-boot-script",
      data: { url: "/otp-assets/missing/bin/vm.boot" },
    },
  });
});

test("handles events in both directions", async ({ otp }) => {
  const BRIDGE_BOOT_EVAL = [
    "spawn(fun() ->",
    "  ok = wasm:send(#{direct => true, nested => #{count => 1}}),",
    "  ok = wasm:register_listener(bridge),",
    "  Loop = fun(F) ->",
    "    ok = wasm:send(#{bridge_ready => true}),",
    "    receive",
    "      {wasm, PayloadJson, MetaJson} ->",
    "        Payload = json:decode(PayloadJson),",
    "        Meta = json:decode(MetaJson),",
    "        ok = wasm:send(#{reply => Payload, meta => Meta})",
    "    after 100 ->",
    "        F(F)",
    "    end",
    "  end,",
    "  Loop(Loop)",
    "end).",
  ].join(" ");
  const boot = await otp.boot({
    beam: {
      assetsUrl: "/otp-assets",
      extraArgs: ["-eval", BRIDGE_BOOT_EVAL],
    },
  });

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

test("surfaces listener lookup failures from popcorn.send", async ({ otp }) => {
  const boot = await otp.boot({
    beam: { assetsUrl: "/otp-assets", extraArgs: ["-eval", READY_BOOT_EVAL] },
  });

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
