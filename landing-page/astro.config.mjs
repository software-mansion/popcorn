// @ts-check
import { defineConfig } from "astro/config";
import { execFile } from "node:child_process";
import { cp, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { crossOriginIsolationHeaders, popcorn } from "@swmansion/popcorn/vite";
import tailwindcss from "@tailwindcss/vite";

const run = promisify(execFile);
const gameOfLifeRoot = fileURLToPath(
  new URL("./game-of-life", import.meta.url),
);
const pongRoot = fileURLToPath(new URL("./pong", import.meta.url));
const publicRoot = fileURLToPath(new URL("./public", import.meta.url));

function compileGameOfLife() {
  return {
    name: "compile-game-of-life",
    hooks: {
      "astro:config:setup": async () => {
        await run("mix", ["deps.get"], { cwd: gameOfLifeRoot });
        await run("mix", ["compile"], { cwd: gameOfLifeRoot });
      },
    },
  };
}

function compilePong() {
  return {
    name: "compile-pong",
    hooks: {
      "astro:config:setup": async () => {
        await run("mise", ["exec", "--", "mix", "deps.get"], {
          cwd: pongRoot,
        });
        await run("mise", ["exec", "--", "mix", "llv.build"], {
          cwd: pongRoot,
        });

        await mkdir(publicRoot, { recursive: true });
        await cp(
          `${pongRoot}/priv/static/assets/js/popcorn`,
          `${publicRoot}/pong`,
          {
            recursive: true,
          },
        );
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
    resolve: {
      alias: {
        // Let Vite resolve Popcorn's worker relative to its package.
        local_live_view: fileURLToPath(
          new URL(
            "../local-live-view/assets/local_live_view/index.ts",
            import.meta.url,
          ),
        ),
      },
    },
    plugins: [
      popcorn({
        rootDir: gameOfLifeRoot,
      }),
      tailwindcss(),
    ],
  },
  integrations: [compileGameOfLife(), compilePong()],
});
