// @ts-check
import { defineConfig } from "astro/config";
import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { crossOriginIsolationHeaders, popcorn } from "@swmansion/popcorn/vite";
import tailwindcss from "@tailwindcss/vite";

const run = promisify(execFile);
const gameOfLifeRoot = fileURLToPath(
  new URL("../examples/game-of-life", import.meta.url),
);

function compileGameOfLife() {
  return {
    name: "compile-game-of-life",
    hooks: {
      "astro:config:setup": async () => {
        await run("mix", ["compile"], { cwd: gameOfLifeRoot });
      },
    },
  };
}

// https://astro.build/config
export default defineConfig({
  site: "https://popcorn.swmansion.com",
  redirects: {
    "/docs": "https://hexdocs.pm/popcorn",
  },
  server: { headers: crossOriginIsolationHeaders },
  experimental: {
    chromeDevtoolsWorkspace: true,
  },
  vite: {
    plugins: [
      popcorn({
        rootDir: gameOfLifeRoot,
        app: "game_of_life",
        extraApps: ["iex", "logger"],
      }),
      tailwindcss(),
    ],
  },
  integrations: [compileGameOfLife()],
});
