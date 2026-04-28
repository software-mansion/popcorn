import { test, expect, type Page } from "@playwright/test";
import type {
  Popcorn,
  SerializedError,
} from "@swmansion/popcorn-otp";

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    Popcorn: typeof Popcorn;
  }
}

type InitSnapshot =
  | { ok: true; data: null }
  | { ok: false; error: SerializedError };

test.beforeEach(async ({ page }) => {
  await page.goto("/");
  await page.waitForFunction(() => window.Popcorn !== undefined);
});

test("boots with the packaged OTP assets through Popcorn.init", async ({
  page,
}) => {
  const result = await initPopcorn(page, "/otp-assets");

  expect(result).toEqual({ ok: true, data: null });
});

test("returns timeout:init through Popcorn.init", async ({ page }) => {
  const result = await initPopcorn(page, "/otp-assets", 1);
  expect(result).toEqual({
    ok: false,
    error: {
      t: "timeout:init",
      data: {
        timeoutMs: 1,
      },
    },
  });
});

test("returns beam:missing-boot-script through Popcorn.init for setup failures", async ({
  page,
}) => {
  const result = await initPopcorn(page, "/otp-assets/missing");
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

async function initPopcorn(
  page: Page,
  assetsUrl: string,
  bootTimeoutMs?: number,
): Promise<InitSnapshot> {
  return await page.evaluate(
    async ({
      beamAssetsUrl,
      initTimeoutMs,
    }: {
      beamAssetsUrl: string;
      initTimeoutMs?: number;
    }): Promise<InitSnapshot> => {
      const result = await window.Popcorn.init({
        beam: { assetsUrl: beamAssetsUrl },
        timeoutsMs:
          initTimeoutMs === undefined ? undefined : { boot: initTimeoutMs },
      });
      if (!result.ok) {
        return { ok: false, error: result.error.serialize() };
      }

      result.data.deinit();
      return { ok: true, data: null };
    },
    {
      beamAssetsUrl: assetsUrl,
      initTimeoutMs: bootTimeoutMs,
    },
  );
}
