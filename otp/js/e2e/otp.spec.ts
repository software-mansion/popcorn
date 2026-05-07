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

const SEND_TO_JS_BOOT_EVAL = [
  "spawn(fun() ->",
  "  ok = wasm:register_listener(direct_sender),",
  "  receive",
  "    {wasm, _PayloadJson, _MetaJson} ->",
  "      ok = wasm:send(#{direct => true, nested => #{count => 1}})",
  "  end",
  "end).",
].join(" ");

test.describe.configure({ mode: "serial" });

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
      const events: Array<PopcornEvent> = [];
      const unsubscribe = popcorn.onEvent((event) => {
        events.push(event);
      });

      let sendResult = popcorn.send("bridge", { ping: true }, {
        meta: { requestId: "req-1" },
      });

      for (let attempt = 0; attempt < 8; attempt += 1) {
        await new Promise((resolve) => setTimeout(resolve, settleMs));

        if (
          events.some(
            (event) =>
              typeof event === "object" &&
              event !== null &&
              "reply" in event,
          )
        ) {
          break;
        }

        sendResult = popcorn.send("bridge", { ping: true }, {
          meta: { requestId: "req-1" },
        });
      }

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
  if (
    !result.data.events.some(
      (event) =>
        typeof event === "object" &&
        event !== null &&
        "reply" in event,
    )
  ) {
    throw new Error(`No bridge reply received: ${JSON.stringify(result.data.events)}`);
  }
  expect(result.data.events).toContainEqual({
    reply: { ping: true },
    meta: { requestId: "req-1" },
  });
});

test("emits wasm:send envelopes as Popcorn events", async ({ page }) => {
  const result = await withPopcorn(
    page,
    {
      beam: {
        assetsUrl: "/otp-assets",
        extraArgs: ["-eval", SEND_TO_JS_BOOT_EVAL],
      },
    },
    { settleMs: 250 },
    async ({ popcorn, settleMs }) => {
      const events: Array<PopcornEvent> = [];
      const unsubscribe = popcorn.onEvent((event) => {
        events.push(event);
      });

      let sendResult = popcorn.send("direct_sender");

      for (let attempt = 0; attempt < 8; attempt += 1) {
        await new Promise((resolve) => setTimeout(resolve, settleMs));

        if (
          events.some(
            (event) =>
              typeof event === "object" &&
              event !== null &&
              "direct" in event,
          )
        ) {
          break;
        }

        sendResult = popcorn.send("direct_sender");
      }

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
    direct: true,
    nested: { count: 1 },
  });
});

test("surfaces listener lookup failures from popcorn.send", async ({ page }) => {
  const consoleErrors: string[] = [];
  const onConsole = (message: { type: () => string; text: () => string }) => {
    if (message.type() === "error" && message.text().includes("[Popcorn]")) {
      consoleErrors.push(message.text());
    }
  };
  page.on("console", onConsole);

  const result = await withPopcorn(
    page,
    {
      beam: {
        assetsUrl: "/otp-assets",
      },
    },
    { settleMs: 250 },
    async ({ popcorn, settleMs }) => {
      await new Promise((resolve) => setTimeout(resolve, settleMs));
      const sendResult = popcorn.send("missing-listener", { ping: true });
      await new Promise((resolve) => setTimeout(resolve, settleMs));

      return {
        send: sendResult.ok
          ? { ok: true }
          : { ok: false, error: sendResult.error.serialize() },
      };
    },
  );
  page.off("console", onConsole);

  assert(result.ok);
  assert(result.data.send.ok);
  const sendErrors = consoleErrors.join("\n");
  expect(sendErrors).toContain("send failed");
  expect(sendErrors).toContain("Target listener not found");
  expect(sendErrors).toContain("missing-listener");
});
