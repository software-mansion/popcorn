// @ts-check
const { test, expect } = require("@playwright/test");

test("HelloLocal renders inside the Phoenix page via live_local route", async ({ page }) => {
  await page.goto("/hello_wasm");

  await expect(page.locator("body")).toContainText("Hello from Wasm!");
  await expect(page.locator("[data-pop-root].phx-connected")).toBeVisible();
});
