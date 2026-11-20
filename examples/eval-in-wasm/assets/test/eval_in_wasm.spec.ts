import { expect, test } from "@playwright/test";

const READY_TIMEOUT = 30_000;

test("case", async ({ page }) => {
  await page.goto("/");
  await page.waitForSelector("[data-popcorn-ready]", { timeout: READY_TIMEOUT });

  await page.getByText("Example: case").click();
  await page.locator("#eval").click();
  await expect(page.locator("#result")).toHaveText("{:ok, 3}");
});

test.skip("module", async ({ page }) => {
  await page.goto("/");
  await page.waitForSelector("[data-popcorn-ready]", { timeout: READY_TIMEOUT });

  await page.getByText("Example: module").click();
  await page.locator("#eval").click();
  await expect(page.locator("#result")).toHaveText("{:sum, 30}");
});
