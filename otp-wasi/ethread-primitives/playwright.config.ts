import { defineConfig } from "@playwright/test";

export default defineConfig({
  testDir: "test",
  timeout: 30_000,
  use: { browserName: "chromium" },
  webServer: {
    command: "pnpm build && pnpm exec vite --host 127.0.0.1 --port 4175",
    cwd: new URL(".", import.meta.url).pathname,
    port: 4175,
    reuseExistingServer: !process.env.CI,
  },
});
