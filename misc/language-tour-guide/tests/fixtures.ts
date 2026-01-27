/* eslint-disable react-hooks/rules-of-hooks */
import { test as base, expect } from "@playwright/test";

type ErrorCollector = {
  pageErrors: Error[];
  consoleErrors: string[];
  assertNoErrors: () => void;
};

export const test = base.extend<{ errorCollector: ErrorCollector }>({
  errorCollector: async ({ page }, use) => {
    const pageErrors: Error[] = [];
    const consoleErrors: string[] = [];

    page.on("pageerror", (error) => pageErrors.push(error));
    page.on("console", (msg) => {
      if (msg.type() === "error") {
        consoleErrors.push(msg.text());
      }
    });

    const collector: ErrorCollector = {
      pageErrors,
      consoleErrors,
      assertNoErrors: () => {
        expect(pageErrors, "Page should have no JS errors").toEqual([]);
        expect(consoleErrors, "Page should have no console errors").toEqual([]);
      }
    };

    await use(collector);
  }
});

export { expect };
