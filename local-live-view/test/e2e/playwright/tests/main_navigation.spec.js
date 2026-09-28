// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

const MAIN = "llv-MainLocal";
const PATCHER = "layout-patcher";

const historyLength = (page) => page.evaluate(() => history.length);

// The same page twice: on /main_live, the root layout also renders a regular
// LiveView, which binds LiveView's own navigation handlers. The local main
// view still owns navigation.
for (const pagePath of ["/main", "/main_live"]) {
  test.describe(`a page whose main view is local (${pagePath})`, () => {
    /** @type {string[]} */
    let errors;

    test.beforeEach(async ({ page }) => {
      errors = h.trackErrors(page);
      await h.open(page, pagePath, MAIN, PATCHER);
      await h.markPage(page);
    });

    test.afterEach(async ({ page }) => {
      await h.expectSamePage(page);
      expect(errors).toEqual([]);
    });

    test("handle_params/3 runs at mount, with the full URL", async ({ page, baseURL }) => {
      await h.open(page, `${pagePath}?tab=b`, MAIN);
      await h.markPage(page);
      await expect(page.locator("#main-tab")).toHaveText("tab: b");
      await expect(page.locator("#main-uri")).toHaveText(`${baseURL}${pagePath}?tab=b`);
      await expect(page.locator("#main-params-calls")).toHaveText("params calls: 1");
    });

    test("push_patch runs handle_params/3 locally", async ({ page, baseURL }) => {
      const length = await historyLength(page);
      await page.locator("#main-patch").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: pushed");
      expect(h.path(page)).toBe(`${pagePath}?tab=pushed`);
      await expect(page.locator("#main-uri")).toHaveText(`${baseURL}${pagePath}?tab=pushed`);
      expect(await historyLength(page)).toBe(length + 1);
    });

    test("push_patch with replace: true replaces the history entry", async ({ page }) => {
      const length = await historyLength(page);
      await page.locator("#main-patch-replace").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: replaced");
      expect(h.path(page)).toBe(`${pagePath}?tab=replaced`);
      expect(await historyLength(page)).toBe(length);
    });

    test("patch links run handle_params/3 without loading the page", async ({ page, baseURL }) => {
      await page.locator("#main-link-a").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: a");
      expect(h.path(page)).toBe(`${pagePath}?tab=a`);
      await expect(page.locator("#main-uri")).toHaveText(`${baseURL}${pagePath}?tab=a`);

      const length = await historyLength(page);
      await page.locator("#main-link-replace").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: link-replaced");
      expect(await historyLength(page)).toBe(length);
    });

    test("a patch link's phx-click runs too", async ({ page }) => {
      await page.locator("#main-link-js").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: js");
      await expect(page.locator("#main-link-js")).toHaveText("link clicks: 1");
    });

    test("back and forward run handle_params/3 locally", async ({ page, baseURL }) => {
      await page.locator("#main-inc").click();
      await page.locator("#main-link-a").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: a");
      await page.locator("#main-patch").click();
      await expect(page.locator("#main-tab")).toHaveText("tab: pushed");

      await page.goBack();
      await expect(page.locator("#main-tab")).toHaveText("tab: a");
      await expect(page.locator("#main-uri")).toHaveText(`${baseURL}${pagePath}?tab=a`);
      await page.goBack();
      await expect(page.locator("#main-tab")).toHaveText("tab: none");
      expect(h.path(page)).toBe(pagePath);
      await page.goForward();
      await expect(page.locator("#main-tab")).toHaveText("tab: a");

      // Still the same view
      await expect(page.locator("#main-inc")).toHaveText("clicks: 1");
    });

    test("another view's push_patch runs the main view's handle_params/3", async ({
      page,
      baseURL,
    }) => {
      await page.locator(`#${PATCHER}-patch`).click();
      await expect(page.locator("#main-tab")).toHaveText("tab: patcher");
      expect(h.path(page)).toBe(`${pagePath}?tab=patcher`);
      await expect(page.locator("#main-uri")).toHaveText(`${baseURL}${pagePath}?tab=patcher`);
      // Only the main view gets handle_params/3
      await expect(page.locator(`#${PATCHER}-params-called`)).toHaveText("params called: false");

      await page.goBack();
      await expect(page.locator("#main-tab")).toHaveText("tab: none");
    });

    test("forms", async ({ page }) => {
      await page.locator("#main-input").fill("hello");
      await expect(page.locator("#main-form-value")).toHaveText("value: hello");
      await page.locator("#main-submit").click();
      await expect(page.locator("#main-submitted")).toHaveText("submitted: hello");
    });
  });
}

test.describe("a page whose main view is local", () => {
  test("the regular LiveView in the layout keeps working across navigation", async ({ page }) => {
    await h.open(page, "/main_live", MAIN, PATCHER);
    await h.markPage(page);

    await page.locator("#layout-live-inc").click();
    await expect(page.locator("#layout-live-inc")).toHaveText("layout live clicks: 1");
    await page.locator("#main-link-a").click();
    await expect(page.locator("#main-tab")).toHaveText("tab: a");
    await page.goBack();
    await expect(page.locator("#main-tab")).toHaveText("tab: none");
    await page.goForward();
    await expect(page.locator("#main-tab")).toHaveText("tab: a");

    await page.locator("#layout-live-inc").click();
    await expect(page.locator("#layout-live-inc")).toHaveText("layout live clicks: 2");
    await h.expectSamePage(page);
  });

  test("push_navigate loads the target page", async ({ page }) => {
    await h.open(page, "/main", MAIN);
    await h.markPage(page);

    await page.locator("#main-navigate").click();
    await page.waitForURL("**/hosted");
    await h.waitForView(page, "llv-Hosted");
    await h.expectNewPage(page);
  });

  test("a patch while the main view is mounting isn't lost", async ({ page }) => {
    // SlowMain takes 3s to mount in the browser. The patch must come from the
    // browser: an event from another view would only be handled once the
    // mount is over, as the Wasm socket handles one join at a time.
    await page.goto("/slow_main");
    await h.waitForView(page, PATCHER);
    // Otherwise, this test doesn't test anything
    expect((await h.root(page, "llv-SlowMain").getAttribute("class")) ?? "").not.toContain(
      "phx-connected",
    );

    await page.locator("#layout-link").click();
    expect(h.path(page)).toBe("/slow_main?tab=layout-link");
    await h.waitForView(page, "llv-SlowMain");
    await expect(page.locator("#slow-main-tab")).toHaveText("tab: layout-link");
  });
});

test.describe("a page with neither a LiveView nor a main view", () => {
  test("push_patch loads the new URL", async ({ page }) => {
    await h.open(page, "/plain", "plain-patcher");
    await h.markPage(page);

    await page.locator("#plain-patcher-patch").click();
    await page.waitForURL("**/plain?x=1");
    await expect(page.locator("#plain-page")).toBeVisible();
    await h.expectNewPage(page);
  });
});
