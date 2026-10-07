// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

test.describe("server-side rendering", () => {
  test("local views are on the page before the Wasm runtime boots, inert", async ({
    browser,
    baseURL,
  }) => {
    const { page, context } = await h.openWithoutWasm(browser, baseURL, "/ssr");

    const content = page.locator("#echo-ssr-content");
    await expect(content.locator(".label")).toHaveText("rendered on the server");
    // Rendered by the server, where connected?/1 is false
    await expect(content.locator(".connected")).toHaveText("connected: false");

    const root = h.root(page, "echo-ssr");
    await expect(root).toHaveAttribute("data-pop-ssr", "");
    // Its events must not reach anything until the local view takes over
    await expect(root).toHaveAttribute("inert", "");
    await expect(root).not.toHaveClass(/phx-connected/);

    await context.close();
  });

  test("llv_ssr={false} renders an empty mount point", async ({ browser, baseURL }) => {
    const { page, context } = await h.openWithoutWasm(browser, baseURL, "/ssr");

    const root = h.root(page, "echo-no-ssr");
    await expect(root).toBeAttached();
    await expect(root).toBeEmpty();
    await expect(root).not.toHaveAttribute("data-pop-ssr");
    await expect(root).not.toHaveAttribute("inert");

    await context.close();
  });

  test("the main view is rendered with handle_params/3 and the full URL", async ({
    browser,
    baseURL,
  }) => {
    const { page, context } = await h.openWithoutWasm(browser, baseURL, "/main?tab=b");

    await expect(page.locator("#main-tab")).toHaveText("tab: b");
    await expect(page.locator("#main-uri")).toHaveText(`${baseURL}/main?tab=b`);
    await expect(page.locator('[data-pop-id="llv-MainLocal"]')).toHaveAttribute("data-pop-main", "");
    // A view that isn't the main one doesn't get handle_params/3
    await expect(page.locator("#layout-patcher-params-called")).toHaveText("params called: false");

    await context.close();
  });

  test("hosted and mirrored views are rendered on the server", async ({ browser, baseURL }) => {
    const hosted = await h.openWithoutWasm(browser, baseURL, "/hosted");
    await expect(hosted.page.locator("#hosted-count")).toHaveText("count: 0");
    await expect(h.root(hosted.page, "llv-Hosted")).toHaveAttribute("data-pop-ssr", "");
    await hosted.context.close();

    // A view with a mirror renders through a live component
    const mirrored = await h.openWithoutWasm(browser, baseURL, "/mirrored");
    await expect(mirrored.page.locator("#mirrored-click")).toHaveText("mirrored clicks: 0");
    await expect(h.root(mirrored.page, "mirrored")).toHaveAttribute("data-pop-ssr", "");
    await mirrored.context.close();
  });

  test("user assigns named like LocalLiveView's own settings are plain assigns", async ({
    browser,
    baseURL,
    page,
  }) => {
    const { page: ssrPage, context } = await h.openWithoutWasm(browser, baseURL, "/ssr");
    const ssrContent = ssrPage.locator("#echo-attrs-content");
    await expect(ssrContent.locator(".main-attr")).toHaveText("main attr: user main");
    await expect(ssrContent.locator(".url-attr")).toHaveText("url attr: user url");
    // ...and don't make the view the page's main view
    await expect(ssrPage.locator('[data-pop-id="echo-attrs"]')).not.toHaveAttribute("data-pop-main");
    await context.close();

    // The same in the browser
    await h.open(page, "/ssr", "echo-attrs");
    const content = page.locator("#echo-attrs-content");
    await expect(content.locator(".connected")).toHaveText("connected: true");
    await expect(content.locator(".main-attr")).toHaveText("main attr: user main");
    await expect(content.locator(".url-attr")).toHaveText("url attr: user url");
    await expect(page.locator('[data-pop-id="echo-attrs"]')).not.toHaveAttribute("data-pop-main");
  });

  test("the local view takes over the server-rendered element", async ({ page }) => {
    // Hold the Wasm bundle back until the server-rendered element is marked
    let release = () => {};
    const released = new Promise((resolve) => (release = resolve));
    await page.route("**/*.avm", async (route) => {
      await released;
      await route.continue();
    });

    await page.goto("/ssr");
    await h.root(page, "echo-ssr").evaluate((el) => {
      el.__llvTestServerRendered = true;
    });
    release();

    await h.waitForView(page, "echo-ssr");
    const root = h.root(page, "echo-ssr");
    // The very element rendered on the server, now live
    expect(await root.evaluate((el) => el.__llvTestServerRendered === true)).toBe(true);
    await expect(root).not.toHaveAttribute("inert");
    await expect(page.locator("#echo-ssr-content .connected")).toHaveText("connected: true");
  });

  test("a view added by a live navigation is rendered on the server too", async ({ page }) => {
    await h.open(page, "/hosted", "llv-Hosted");
    await h.markPage(page);

    // SlowLocal takes 3s to mount in the browser
    await page.locator("#host-link-slow").click();
    await expect(page.locator("#slow-host")).toBeVisible();
    const before = await page.evaluate(() => ({
      text: document.querySelector("#slow-local")?.textContent,
      connected: document
        .querySelector('[data-pop-id="llv-SlowLocal"] [data-pop-root]')
        ?.classList.contains("phx-connected"),
    }));
    expect(before).toEqual({ text: "slow content, connected: false", connected: false });

    await expect(page.locator("#slow-local")).toHaveText("slow content, connected: true");
    await h.expectSamePage(page);
  });
});
