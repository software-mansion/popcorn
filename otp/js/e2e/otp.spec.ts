import { test, expect, type Page } from "@playwright/test";
import type { Popcorn, SerializedError } from "@swmansion/popcorn-otp";

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    Popcorn: typeof Popcorn;
  }
}

type InitSnapshot =
  | { ok: true }
  | {
      ok: false;
      error: SerializedError;
    };

test.beforeEach(async ({ page }) => {
  await page.goto("/");
  await page.waitForFunction(() => window.Popcorn !== undefined);
});

test("boots with the packaged OTP assets through Popcorn.init", async ({
  page,
}) => {
  const result = await initPopcorn(page, "/otp-assets");

  expect(result.ok).toBe(true);
});

test("reports a missing boot script through Popcorn.init", async ({ page }) => {
  const result = await initPopcorn(page, "/otp-assets/missing");
  expect(result.ok).toBe(false);
  if (result.ok) {
    throw new Error("expected Popcorn.init to fail");
  }

  expect(result.error).toEqual({
    t: "worker:load",
    message: "bad-fs",
  });
});

async function initPopcorn(
  page: Page,
  assetsUrl: string,
): Promise<InitSnapshot> {
  return await page.evaluate(async (beamAssetsUrl): Promise<InitSnapshot> => {
    const result = await window.Popcorn.init({
      beam: { assetsUrl: beamAssetsUrl },
    });
    if (!result.ok) {
      return { ok: false, error: result.error.serialize() };
    }

    result.popcorn.deinit();
    return { ok: true };
  }, assetsUrl);
}
