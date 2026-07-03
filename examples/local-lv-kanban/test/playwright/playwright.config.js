// @ts-check
const { defineConfig } = require("@playwright/test");

/**
 * E2E tests for the collaborative kanban example.
 *
 * This config does NOT start a web server. The Phoenix endpoint is owned by the
 * ExUnit test that wraps this suite (`test/e2e_test.exs`): it boots the endpoint
 * in-process on :4901 (MIX_ENV=test, shared SQL sandbox) and then runs `pnpm test`.
 *
 * Run the suite via Elixir: `mix test` (or `mix test --only e2e`) from the app root.
 * To run Playwright directly (`pnpm test`) you must already have a server listening
 * on http://localhost:4901.
 */
module.exports = defineConfig({
  testDir: "./tests",
  // The board renders inside a WASM (Popcorn) runtime that boots per page; first
  // mount can take several seconds, so timeouts are generous.
  // Generous timeouts: the WASM local live view runs in-browser and competes with the
  // server + browser for CPU on constrained CI runners.
  timeout: 120_000,
  expect: { timeout: 30_000 },
  // Tests share one server + DB and exercise PubSub broadcasts, so run serially.
  fullyParallel: false,
  workers: 1,
  retries: process.env.CI ? 2 : 0,
  // `list` for live console output; `html` produces a self-contained report
  // (with traces/screenshots/video) that CI uploads as an artifact so failures
  // are actually diagnosable — the list output alone never says *why* a test
  // timed out or which locator it was waiting on.
  reporter: [["list"], ["html", { open: "never" }]],
  use: {
    baseURL: "http://localhost:4901",
    browserName: "chromium",
    // CI sets PW_CHANNEL=chrome to use the runner's preinstalled Google Chrome
    // (no browser download — Playwright's chromium download hangs on the runner).
    // Unset locally → Playwright's bundled chromium.
    channel: process.env.PW_CHANNEL || undefined,
    headless: true,
    actionTimeout: 30_000,
    navigationTimeout: 45_000,
    // No `video`: it needs Playwright's bundled ffmpeg, which isn't installed
    // (we run via `channel: chrome`, with no `playwright install`). Trace +
    // screenshot need no extra binaries and are enough to diagnose failures.
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
  },
});
