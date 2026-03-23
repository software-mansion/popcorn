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
        await page.keyboard.press("ControlOrMeta+A");
        await page.keyboard.insertText(testReplaceCode!);
      }

      const runButtons = page.locator("button").filter({ hasText: "Run Code" });
      await runButtons.last().click();

      // TODO: support data-test-expect-failure attribute on code cells
      // to allow editors that are expected to fail
      const badges = page.locator("[data-testid='execution-state-badge']");
      const count = await badges.count();
      for (let i = 0; i < count; i++) {
        await expect(async () => {
          const state = await badges.nth(i).getAttribute("data-state");
          expect(state).not.toBe("not_run");
          expect(state).not.toBe("running");
          expect(state).not.toBe("queued");
        }).toPass({ timeout: 90_500 });

        await expect(badges.nth(i)).toHaveAttribute("data-state", "success");
      }

      errorCollector.assertNoErrors();
    });
  }
});
