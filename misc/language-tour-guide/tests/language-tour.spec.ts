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

      const fixableEditors = page.locator("[data-correct-code]");
      const fixCount = await fixableEditors.count();
      for (let i = 0; i < fixCount; i++) {
        const cell = fixableEditors.nth(i);
        const correctCode = await cell.getAttribute("data-correct-code");
        const cmContent = cell.locator(".cm-content");

        // dispatch change to CodeMirror instance to prevent auto closing
        await cmContent.evaluate((el, code) => {
          const view = (el as any).cmView.view;
          view.dispatch({
            changes: {
              from: 0,
              to: view.state.doc.length,
              insert: code
            }
          });
        }, correctCode!);
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
      }).toPass({ timeout: 30000 });

      const badges = page.locator("[data-testid='execution-state-badge']");
      const count = await badges.count();
      for (let i = 0; i < count; i++) {
        await expect(badges.nth(i)).toHaveAttribute("data-state", "success");
      }

      errorCollector.assertNoErrors();
    });
  }
});
