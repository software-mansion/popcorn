import { test, expect, type Page } from "@playwright/test";
import type { Popcorn } from "@swmansion/popcorn-otp";

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
      error: {
        kind: string;
        message: string;
        metadata: Record<string, unknown>;
      };
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
  check(!result.ok);
  expect(result.error.kind).toBe("boot-failed");
  expect(result.error.metadata.reason).toBe("missing-boot-script");
  expect(result.error.metadata.url).toBe("/otp-assets/missing/bin/vm.boot");
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
      return {
        ok: false,
        error: {
          kind: result.error.kind,
          message: result.error.message,
          metadata: result.error.metadata,
        },
      };
    }

    result.popcorn.deinit();
    return { ok: true };
  }, assetsUrl);
}

function check(ok: boolean): asserts ok {
  if (!ok) throw new Error("assert");
}
