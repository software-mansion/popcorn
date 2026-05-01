import type { PopcornEvent } from "@swmansion/popcorn-otp";
import { assert, expect, test, withPopcorn } from "./helpers";

const BRIDGE_BOOT_EVAL = [
  "spawn(fun() ->",
  "  ok = wasm:register_listener(bridge),",
  "  Loop = fun(F) ->",
  "    receive",
  "      {wasm, PayloadJson, MetaJson} ->",
  "        Payload = json:decode(PayloadJson),",
  "        Meta = json:decode(MetaJson),",
  "        ok = wasm:send(#{reply => Payload, meta => Meta}),",
  "        F(F)",
  "    end",
  "  end,",
  "  Loop(Loop)",
  "end).",
].join(" ");

const WASM_LOAD_EVAL = "code:ensure_loaded(wasm).";

test("boots with the packaged OTP assets through Popcorn.init", async ({
  page,
}) => {
  const result = await withPopcorn(
    page,
    { beam: { assetsUrl: "/otp-assets" } },
    {},
    async () => null,
  );

  expect(result).toEqual({ ok: true, data: null });
});

test("returns timeout:init through Popcorn.init", async ({ page }) => {
  const result = await withPopcorn(
    page,
    { beam: { assetsUrl: "/otp-assets" }, timeoutsMs: { boot: 0 } },
    {},
    async () => null,
  );
  expect(result).toEqual({
    ok: false,
    error: {
      t: "timeout:init",
      data: {
        timeoutMs: 0,
      },
    },
  });
});

test("returns beam:missing-boot-script through Popcorn.init for setup failures", async ({
  page,
}) => {
  const result = await withPopcorn(
    page,
    { beam: { assetsUrl: "/otp-assets/missing" } },
    {},
    async () => null,
  );
  expect(result).toEqual({
    ok: false,
    error: {
      t: "beam:missing-boot-script",
      data: {
        url: "/otp-assets/missing/bin/vm.boot",
      },
    },
  });
});

test("round-trips through the native bridge when a wasm listener is registered", async ({
  page,
}) => {
  const result = await withPopcorn(
    page,
    {
      beam: {
        assetsUrl: "/otp-assets",
        extraArgs: ["-eval", BRIDGE_BOOT_EVAL],
      },
    },
    { settleMs: 250 },
    async ({ popcorn, settleMs }) => {
      const messages: unknown[] = [];
      const events: Array<PopcornEvent> = [];
      const unsubscribe = popcorn.onMessage((message) => {
        messages.push(message);
      });
      const unsubscribeEvents = popcorn.onEvent((event) => {
        events.push(event);
      });

      let sendResult = popcorn.send("bridge", { ping: true }, {
        meta: { requestId: "req-1" },
      });

      for (let attempt = 0; attempt < 8; attempt += 1) {
        await new Promise((resolve) => setTimeout(resolve, settleMs));

        if (
          messages.some(
            (message) =>
              typeof message === "object" &&
              message !== null &&
              "reply" in message,
          )
        ) {
          break;
        }

        sendResult = popcorn.send("bridge", { ping: true }, {
          meta: { requestId: "req-1" },
        });
      }

      unsubscribe();
      unsubscribeEvents();
      return {
        send: sendResult.ok
          ? { ok: true }
          : { ok: false, error: sendResult.error.serialize() },
        messages,
        events,
      };
    },
  );

  assert(result.ok);
  assert(result.data.send.ok);
  if (
    !result.data.messages.some(
      (message) =>
        typeof message === "object" &&
        message !== null &&
        "reply" in message,
    )
  ) {
    throw new Error(
      `No bridge reply received. Events: ${JSON.stringify(result.data.events)}`,
    );
  }
  expect(result.data.messages).toContainEqual({
    reply: { ping: true },
    meta: { requestId: "req-1" },
  });
});

test("surfaces listener lookup failures from popcorn.send", async ({ page }) => {
  const result = await withPopcorn(
    page,
    {
      beam: {
        assetsUrl: "/otp-assets",
        extraArgs: ["-eval", WASM_LOAD_EVAL],
      },
    },
    { settleMs: 250 },
    async ({ popcorn, settleMs }) => {
      const events: Array<PopcornEvent> = [];
      const unsubscribe = popcorn.onEvent((event) => {
        events.push(event);
      });

      await new Promise((resolve) => setTimeout(resolve, settleMs));
      const sendResult = popcorn.send("missing-listener", { ping: true });
      await new Promise((resolve) => setTimeout(resolve, settleMs));

      unsubscribe();
      return {
        send: sendResult.ok
          ? { ok: true }
          : { ok: false, error: sendResult.error.serialize() },
        events,
      };
    },
  );

  assert(result.ok);
  assert(result.data.send.ok);
  expect(result.data.events).toContainEqual({
    type: "otp:error",
    payload:
      '{"type":"listener_not_found","detail":"target listener is not registered"}',
  });
});
