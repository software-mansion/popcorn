// @ts-check
import { defineConfig } from "astro/config";
import { fileURLToPath } from "node:url";

import { popcorn } from "@swmansion/popcorn/vite";
import tailwindcss from "@tailwindcss/vite";

// https://astro.build/config
export default defineConfig({
  site: "https://popcorn.swmansion.com",
  redirects: {
    "/docs": "https://hexdocs.pm/popcorn",
  },
  experimental: {
    chromeDevtoolsWorkspace: true,
  },
  vite: {
    plugins: [
      popcorn({
        rootDir: fileURLToPath(new URL("../examples/iex-wasm", import.meta.url)),
        app: "iex",
        extraApps: ["logger"],
      }),
      tailwindcss(),
    ],
  },
});
