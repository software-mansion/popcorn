import { test, expect } from "./fixtures";
import { getRoutes } from "../src/utils/content/navigation-structure";

const tourRoutes = getRoutes();

test.describe("Language Tour - All Topics", () => {
  for (const route of tourRoutes) {
    test(`should run code on ${route} without errors`, async ({
      page,
      errorCollector
    }) => {
      const response = await page.goto(`http://localhost:5173${route}`);

      expect(response?.status()).toBe(200);

      await page.waitForLoadState();

      const mdxWrapper = page.locator("[data-testid='mdx-wrapper']");
      await mdxWrapper.waitFor({ state: "visible" });

      const fixableEditors = page.locator("[data-test-replace-code]");
      const fixCount = await fixableEditors.count();
      for (let i = 0; i < fixCount; i++) {
        const cell = fixableEditors.nth(i);
        const testReplaceCode = await cell.getAttribute(
          "data-test-replace-code"
        );

        const cmContent = cell.locator(".cm-content");

        await cmContent.click();
        await page.keyboard.press("Meta+A");
        await cmContent.evaluate((el, code) => {
          const dt = new DataTransfer();
          dt.setData("text/plain", code!);
          el.dispatchEvent(
            new ClipboardEvent("paste", {
              clipboardData: dt,
              bubbles: true,
              cancelable: true
            })
          );
        }, testReplaceCode);
      }

      const runButtons = page.locator("button").filter({ hasText: "Run Code" });
      await runButtons.last().click();

      await expect(async () => {
        const badges = page.locator("[data-testid='execution-state-badge']");
        const count = await badges.count();
        for (let i = 0; i < count; i++) {
          const state = await badges.nth(i).getAttribute("data-state");
          expect(state).not.toBe("running");
          expect(state).not.toBe("queued");
        }
      }).toPass({ timeout: 30_000 });

      // TODO: support data-test-expect-failure attribute on code cells
      // to allow editors that are expected to fail
      const badges = page.locator("[data-testid='execution-state-badge']");
      const count = await badges.count();
      for (let i = 0; i < count; i++) {
        await expect(badges.nth(i)).toHaveAttribute("data-state", "success", {
          timeout: 90_000
        });
      }

      errorCollector.assertNoErrors();
    });
  }
});
