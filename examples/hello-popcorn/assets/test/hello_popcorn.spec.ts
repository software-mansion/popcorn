import { expect, test } from "@playwright/test";

const READY_TIMEOUT = 30_000;

test("says hello from WASM", async ({ page }) => {
  await page.goto("/");

  await expect(page.locator("body")).toHaveText("Hello from WASM!", {
    timeout: READY_TIMEOUT,
  });
});

test("says hello to the console", async ({ page }) => {
  const messages: string[] = [];
  page.on("console", (msg) => messages.push(msg.text()));

  await page.goto("/");

  await expect
    .poll(() => messages.filter((text) => text.includes("Hello console!")).length, {
      timeout: READY_TIMEOUT,
    })
    .toBe(1);
});
