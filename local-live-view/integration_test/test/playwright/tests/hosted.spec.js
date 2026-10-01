// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

// The Hosted local view, inside the HostLive LiveView. Each test gets its own
// room, the scope of the host's broadcasts.
const openHosted = async (page) => {
  await h.open(page, `/hosted?room=${test.info().testId}`, "llv-Hosted");
};

test.describe("a view inside a host LiveView", () => {
  test("gets the host's assigns through update/2", async ({ page }) => {
    await openHosted(page);
    await expect(page.locator("#hosted-count")).toHaveText("count: 0");

    await page.locator("#host-inc").click();
    await expect(page.locator("#host-count")).toHaveText("host count: 1");
    await expect(page.locator("#hosted-count")).toHaveText("count: 1");
  });

  test("push_server_event: the host confirms an optimistic edit", async ({ page }) => {
    await openHosted(page);

    await page.locator("#hosted-inc").click();
    await expect(page.locator("#hosted-count")).toHaveText("count: 1");
    await expect(page.locator("#host-count")).toHaveText("host count: 1");
  });

  test("push_server_event: the host rejects an optimistic edit, which rolls back", async ({
    page,
  }) => {
    await openHosted(page);

    // The optimistic +100 can be rolled back too quickly to poll for
    const sawOptimistic = await h.watchForText(page, "count: 100");
    await page.locator("#hosted-reject").click();
    await sawOptimistic();
    await expect(page.locator("#hosted-count")).toHaveText("count: 0");
    await expect(page.locator("#host-count")).toHaveText("host count: 0");
  });

  test("handle_push_error: an optimistic edit rolls back when the push fails", async ({
    page,
  }) => {
    await openHosted(page);

    // Without the host's websocket, push_server_event fails
    await page.evaluate(() => window.liveSocket.disconnect());
    const sawOptimistic = await h.watchForText(page, "count: 1");
    await page.locator("#hosted-inc").click();
    await sawOptimistic();
    // The default handle_push_error/4 feeds the last server assigns to update/2
    await expect(page.locator("#hosted-count")).toHaveText("count: 0");
  });

  test("forms", async ({ page }) => {
    await openHosted(page);

    await page.locator("#hosted-input").fill("hello");
    await expect(page.locator("#hosted-form-value")).toHaveText("value: hello");
    await page.locator("#hosted-submit").click();
    await expect(page.locator("#hosted-submitted")).toHaveText("submitted: hello");
  });

  test("phx-drag* and phx-mouse* bindings, with pointer data", async ({ page }) => {
    await openHosted(page);

    // Playwright's dragTo doesn't fire HTML5 drag events, so dispatch them
    for (const type of ["dragstart", "dragover", "dragend"]) {
      await page.locator("#hosted-draggable").evaluate((el, type) => {
        const init = { bubbles: true, cancelable: true, dataTransfer: new DataTransfer() };
        el.dispatchEvent(new DragEvent(type, { ...init, clientX: 10, clientY: 10 }));
      }, type);
      // The events are handled in order, one Wasm round trip each
      await page.waitForTimeout(200);
    }
    await expect(page.locator("#hosted-drag-events")).toHaveText(
      "drag_start:true,drag_over:true,drag_end:true",
    );

    await page.locator("#hosted-mouse").click();
    await expect(page.locator("#hosted-mouse-event")).toHaveText("mouse_down:true");
  });

  test("host updates reach other clients' views", async ({ browser, baseURL }) => {
    const url = `/hosted?room=${test.info().testId}`;
    const contexts = [await browser.newContext({ baseURL }), await browser.newContext({ baseURL })];

    try {
      const [a, b] = await Promise.all(contexts.map((context) => context.newPage()));
      await h.open(a, url, "llv-Hosted");
      await h.open(b, url, "llv-Hosted");

      await a.locator("#hosted-inc").click();
      await expect(b.locator("#host-count")).toHaveText("host count: 1");
      await expect(b.locator("#hosted-count")).toHaveText("count: 1");
    } finally {
      await Promise.all(contexts.map((context) => context.close()));
    }
  });

  test("a crashed view remounts and keeps working", async ({ page }) => {
    await openHosted(page);
    await page.locator("#host-inc").click();
    await expect(page.locator("#hosted-count")).toHaveText("count: 1");

    // An event no handle_event/3 clause matches crashes the view's process.
    // LiveView then rejoins, mounting it again.
    const pushed = await page.evaluate(() => {
      const view = Object.values(window.liveSocket.roots ?? {}).find((view) =>
        view.el.closest?.('[data-pop-id="llv-Hosted"]'),
      );
      view?.channel.push("event", { type: "click", event: "__crash__", value: {} });
      return view !== undefined;
    });
    expect(pushed).toBe(true);

    const root = h.root(page, "llv-Hosted");
    await expect(root).toHaveClass(/phx-error/);
    await expect(root).not.toHaveClass(/phx-error/);

    // Remounted with the host's assigns, and live
    await expect(page.locator("#hosted-count")).toHaveText("count: 1");
    await page.locator("#hosted-local-inc").click();
    await expect(page.locator("#hosted-local-inc")).toHaveText("local clicks: 1");
  });

  test("navigating away live tears the views down, coming back mounts them again", async ({
    page,
  }) => {
    const errors = h.trackErrors(page);
    await h.open(page, "/hosted", "llv-Hosted", "only-on-one");
    await h.markPage(page);

    await page.locator("#host-link-other").click();
    await expect(page.locator("#other-page")).toBeVisible();
    await h.expectSamePage(page);
    await expect.poll(() => h.zombieViews(page)).toEqual([]);
    await expect.poll(() => h.mountPointsInMain(page)).toEqual([]);

    await page.goBack();
    await h.waitForView(page, "llv-Hosted");
    await h.waitForView(page, "only-on-one");
    await h.expectSamePage(page);
    await page.locator("#hosted-local-inc").click();
    await expect(page.locator("#hosted-local-inc")).toHaveText("local clicks: 1");
    expect(errors).toEqual([]);
  });

  test("mirror_sync reaches the view's server-side mirror", async ({ page }) => {
    await h.open(page, "/mirrored", "mirrored");

    await page.locator("#mirrored-click").click();
    await expect(page.locator("#mirrored-click")).toHaveText("mirrored clicks: 1");
    await expect(page.locator("#mirror-synced")).toHaveText("synced: 1");
  });
});
