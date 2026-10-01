// @ts-check
const { expect } = require("@playwright/test");

/** The mount point (root) of the local view with the given id. */
const root = (page, id) => page.locator(`[data-pop-id="${id}"] [data-pop-root]`);

/** Waits until the local view with the given id has connected in the browser. */
async function waitForView(page, id) {
  // The Wasm runtime boots on every page load
  await expect(root(page, id)).toHaveClass(/phx-connected/, { timeout: 20_000 });
}

/** Loads `path` and waits for the given local views to connect. */
async function open(page, path, ...ids) {
  await page.goto(path);
  for (const id of ids) await waitForView(page, id);
}

const pageLoads = new WeakMap();

/**
 * Marks the current page, see `expectSamePage`. A full page load wipes the
 * mark, so it tells a navigation in place from one that loaded a new page.
 */
async function markPage(page) {
  await page.evaluate(() => {
    window.__llvTestMark = true;
  });
  if (!pageLoads.has(page)) {
    page.on("request", (request) => {
      if (request.isNavigationRequest() && request.frame() === page.mainFrame()) {
        pageLoads.set(page, pageLoads.get(page) + 1);
      }
    });
  }
  pageLoads.set(page, 0);
}

/** No page was loaded since `markPage`, not even one that's still loading. */
async function expectSamePage(page) {
  // A page load can start a moment after the navigation that caused it,
  // e.g. once LiveView gives up navigating in place.
  await page.waitForTimeout(1000);
  expect(pageLoads.get(page), "page loads since markPage").toBe(0);
  expect(await page.evaluate(() => window.__llvTestMark === true)).toBe(true);
}

async function expectNewPage(page) {
  expect(await page.evaluate(() => window.__llvTestMark === true)).toBe(false);
}

/** The page's path and query. */
const path = (page) => {
  const url = new URL(page.url());
  return url.pathname + url.search;
};

/** Collects console errors and uncaught exceptions of the page. */
function trackErrors(page) {
  const errors = [];
  page.on("console", (m) => {
    if (m.type() === "error") errors.push(m.text().split("\n")[0]);
  });
  page.on("pageerror", (e) => errors.push(e.message));
  return errors;
}

/**
 * Local views whose client-side view outlived its element: the element left
 * the page, but the view is still registered in the LiveSocket.
 */
async function zombieViews(page) {
  return page.evaluate(() =>
    Object.values(window.liveSocket.roots ?? {})
      .filter((view) => view.el?.hasAttribute?.("data-pop-root") && !view.el.isConnected)
      .map((view) => view.id),
  );
}

/** Mount points inside the page's main LiveView, as opposed to the layout. */
async function mountPointsInMain(page) {
  return page.evaluate(() =>
    Array.from(document.querySelectorAll("[data-phx-main] [data-pop-root]")).map((el) => el.id),
  );
}

/** Records whether `text` ever appears on the page, for changes too quick to poll. */
async function watchForText(page, text) {
  await page.evaluate((text) => {
    window.__llvTestSeen = window.__llvTestSeen || {};
    window.__llvTestSeen[text] = false;
    new MutationObserver(() => {
      if (document.body.textContent.includes(text)) window.__llvTestSeen[text] = true;
    }).observe(document.body, { childList: true, subtree: true, characterData: true });
  }, text);
  return () => page.waitForFunction((text) => window.__llvTestSeen[text], text);
}

/** Opens a page where the Wasm runtime never loads: what's there comes from the server. */
async function openWithoutWasm(browser, baseURL, path) {
  const context = await browser.newContext({ baseURL });
  const page = await context.newPage();
  await page.route("**/*.avm", (route) => route.abort());
  await page.goto(path);
  return { page, context };
}

module.exports = {
  root,
  waitForView,
  open,
  markPage,
  expectSamePage,
  expectNewPage,
  path,
  trackErrors,
  zombieViews,
  mountPointsInMain,
  watchForText,
  openWithoutWasm,
};
