// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

const HOSTED = "llv-Hosted";

test.describe("navigation on a page with a host LiveView", () => {
  /** @type {string[]} */
  let errors;

  test.beforeEach(async ({ page }) => {
    errors = h.trackErrors(page);
    await h.open(page, "/hosted", HOSTED, "only-on-one", "layout-patcher");
    await h.markPage(page);
  });

  test.afterEach(async ({ page }) => {
    await h.expectSamePage(page);
    expect(errors).toEqual([]);
  });

  test("push_patch runs the host's handle_params/3 on the server", async ({ page }) => {
    await page.locator("#hosted-patch").click();
    await expect(page.locator("#host-tab")).toHaveText("host tab: hosted-patch");
    await expect(page.locator("#hosted-tab")).toHaveText("tab: hosted-patch");
    expect(h.path(page)).toBe("/hosted?tab=hosted-patch");

    await page.goBack();
    await expect(page.locator("#host-tab")).toHaveText("host tab: none");
    expect(h.path(page)).toBe("/hosted");
  });

  test("a view outside the host LiveView patches the host", async ({ page }) => {
    await page.locator("#layout-patcher-patch").click();
    await expect(page.locator("#host-tab")).toHaveText("host tab: patcher");
    expect(h.path(page)).toBe("/hosted?tab=patcher");
    await expect(page.locator("#layout-patcher-params-called")).toHaveText("params called: false");
  });

  test("push_navigate from handle_event/3 navigates live", async ({ page }) => {
    await page.locator("#hosted-navigate").click();
    await expect(page.locator("#host-page")).toHaveText("page: two");
    expect(h.path(page)).toBe("/hosted2");
    await expectLive(page);
    await expect(h.root(page, "only-on-one")).toHaveCount(0);
  });

  test("push_navigate from handle_info/2 navigates live", async ({ page }) => {
    await page.locator("#hosted-navigate-later").click();
    await expect(page.locator("#host-page")).toHaveText("page: two");
    await expectLive(page);

    await page.locator("#hosted-navigate-later").click();
    await expect(page.locator("#host-page")).toHaveText("page: one");
    expect(h.path(page)).toBe("/hosted");
    await expectLive(page);
    await h.waitForView(page, "only-on-one");
  });

  test("live navigation between pages rendering the same view", async ({ page }) => {
    await page.locator("#host-link-navigate").click();
    await expect(page.locator("#host-page")).toHaveText("page: two");
    await expectLive(page);

    await page.locator("#host-link-navigate").click();
    await expect(page.locator("#host-page")).toHaveText("page: one");
    await expectLive(page);

    await page.goBack();
    await expect(page.locator("#host-page")).toHaveText("page: two");
    await expectLive(page);

    await page.goForward();
    await expect(page.locator("#host-page")).toHaveText("page: one");
    await expectLive(page);
    await h.waitForView(page, "only-on-one");
  });
});

// The page's Hosted view is mounted and live, and nothing of the previous
// page's views is left
async function expectLive(page) {
  await h.waitForView(page, HOSTED);
  const clicks = page.locator("#hosted-local-inc");
  await expect(clicks).toHaveText("local clicks: 0");
  await clicks.click();
  await expect(clicks).toHaveText("local clicks: 1");
  await expect.poll(() => h.zombieViews(page)).toEqual([]);
}
