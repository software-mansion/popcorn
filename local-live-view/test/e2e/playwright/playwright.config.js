// @ts-check
const { defineConfig } = require("@playwright/test");

/**
 * End-to-end tests for LocalLiveView.
 *
 * This config doesn't start a server: test/e2e/e2e_test.exs serves the app
 * from local_live_view's test VM on :4904 and runs this suite, see
 * test/e2e/README.md. To run the suite against a server started by hand, set
 * BASE_URL.
 */
module.exports = defineConfig({
  testDir: "./tests",
  // Short timeouts, so that a failing test fails fast. Passing tests take a
  // few seconds at most.
  timeout: 30_000,
  expect: { timeout: 10_000 },
  // One server, and some tests share server-side state (PubSub rooms).
  fullyParallel: false,
  workers: 1,
  retries: process.env.CI ? 2 : 0,
  reporter: [["list"], ["html", { open: "never" }]],
  use: {
    baseURL: process.env.BASE_URL || "http://localhost:4904",
    browserName: "chromium",
    // CI sets PW_CHANNEL=chrome to use the runner's preinstalled Chrome.
    channel: process.env.PW_CHANNEL || undefined,
    headless: true,
    actionTimeout: 10_000,
    navigationTimeout: 15_000,
    trace: "retain-on-failure",
    screenshot: "only-on-failure",
  },
});
