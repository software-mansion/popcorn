// @ts-check
import { defineConfig } from "@playwright/test";

// The example as `mix popcorn.dev` cooked it, hosted by serve.exs, as a static
// host would. LIVEBOOK_URL tests another server instead.
const port = 4002;
const url = process.env.LIVEBOOK_URL ?? `http://localhost:${port}/`;

export default defineConfig({
  testDir: "./tests",
  // Livebook boots in a browser VM, and notebooks install packages from Hex
  timeout: 300_000,
  expect: { timeout: 60_000 },
  // The tests share one Livebook, its notebooks and their VMs
  workers: 1,
  reporter: [["list"], ["html", { open: "never" }]],
  use: {
    baseURL: url,
    browserName: "chromium",
    viewport: { width: 1280, height: 900 },
  },
  webServer: process.env.LIVEBOOK_URL
    ? undefined
    : {
        command: `elixir serve.exs ${port}`,
        cwd: "../..",
        url,
        reuseExistingServer: !process.env.CI,
        // serve.exs installs its dependencies
        timeout: 180_000,
      },
});
