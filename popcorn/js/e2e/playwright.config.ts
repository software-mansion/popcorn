import { defineConfig, devices } from "@playwright/test";
import { fileURLToPath } from "url";

const jsRootDir = fileURLToPath(new URL("..", import.meta.url));
const buildCommand = process.env.POPCORN_PREBUILT_RUNTIME
  ? "pnpm run build:release"
  : "pnpm run build";
const webServerCommand = [
  buildCommand,
  "cd e2e/elixir",
  "mix deps.get",
  "mix popcorn.cook",
  "cd ../setup",
  "exec pnpm exec vite",
].join(" && ");

export default defineConfig({
  testDir: ".",
  fullyParallel: true,
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: "html",
  use: {
    baseURL: "http://localhost:5173",
    trace: "on-first-retry",
  },
  projects: [
    {
      name: "chromium",
      use: { ...devices["Desktop Chrome"] },
    },
  ],
  webServer: {
    command: webServerCommand,
    cwd: jsRootDir,
    url: "http://localhost:5173",
    reuseExistingServer: !process.env.CI,
    timeout: 120_000,
  },
  timeout: 60000,
});
