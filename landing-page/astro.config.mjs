// @ts-check
import { defineConfig } from "astro/config";

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
    plugins: [tailwindcss()],
  },
});
