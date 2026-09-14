// @ts-check
const { test, expect } = require("@playwright/test");
const h = require("./helpers");

// The mount point rendered on the server, marked by the component.
const SSR_ROOT = "[data-pop-root][data-pop-ssr]";
const SEEDED = ["To Do", "In Progress", "Done"];

test.describe("server-side rendering", () => {
  test("the board is on the page before the Wasm runtime boots", async ({ page, browser }) => {
    const url = await h.createBoard(page);

    // A fresh page that never receives the Wasm bundle: what the user sees
    // while the runtime is still loading.
    const context = await browser.newContext();
    const blocked = await context.newPage();
    await blocked.route("**/*.avm", (route) => route.abort());
    await blocked.goto(url);

    await expect(blocked.locator(SSR_ROOT)).toBeVisible();
    expect(await h.columnNames(blocked)).toEqual(SEEDED);
    // ...but inert until the local view takes over: its events must not
    // reach the host LiveView.
    await expect(blocked.locator(SSR_ROOT)).toHaveAttribute("inert", "");
    await expect(blocked.locator(SSR_ROOT)).not.toHaveClass(/phx-connected/);

    await context.close();
  });

  test("the local view takes over the server-rendered element", async ({ page }) => {
    await h.createBoard(page);

    // The element rendered on the server is the one the view connected on: it
    // kept its marker and got LiveView's connected class, instead of being replaced.
    await expect(page.locator(`${SSR_ROOT}.phx-connected`)).toBeVisible();
    await expect(page.locator(SSR_ROOT)).not.toHaveAttribute("inert");
    expect(await h.columnNames(page)).toEqual(SEEDED);

    // ...and it is live.
    await h.addColumn(page, "After hydration");
    expect(await h.columnNames(page)).toEqual([...SEEDED, "After hydration"]);
  });
});
