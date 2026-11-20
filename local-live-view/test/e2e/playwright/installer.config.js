// @ts-check
const { defineConfig } = require("@playwright/test");

module.exports = defineConfig({
  testDir: "./installer",
  timeout: 90_000,
  expect: { timeout: 60_000 },
  workers: 1,
  retries: process.env.CI ? 2 : 0,
  reporter: [["list"]],
  use: {
    baseURL: process.env.BASE_URL || "http://localhost:4567",
    browserName: "chromium",
    channel: process.env.PW_CHANNEL || undefined,
    headless: true,
  },
});
