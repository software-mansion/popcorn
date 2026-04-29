import type { PopcornEvent } from "@swmansion/popcorn-otp";
import { assert, expect, test, withPopcorn } from "./helpers";

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

test("reaches the native bridge from popcorn.send and surfaces bridge readiness errors", async ({
  page,
}) => {
  const result = await withPopcorn(
    page,
    { beam: { assetsUrl: "/otp-assets" } },
    { settleMs: 250 },
    async ({ popcorn, settleMs }) => {
      const events: Array<PopcornEvent> = [];
      const unsubscribe = popcorn.onEvent((event) => {
        events.push(event);
      });

      const sendResult = popcorn.send("", { ping: true });
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
    payload: '{"type":"bridge_not_ready","detail":"bridge port is not open"}',
  });
});
