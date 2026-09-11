// @ts-check
import { defineConfig } from "astro/config";
import { execFile } from "node:child_process";
import { copyFile, mkdir } from "node:fs/promises";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { crossOriginIsolationHeaders, popcorn } from "@swmansion/popcorn/vite";
import tailwindcss from "@tailwindcss/vite";

const run = promisify(execFile);
const gameOfLifeRoot = fileURLToPath(
  new URL("./game-of-life", import.meta.url),
);
const pongRoot = fileURLToPath(
  new URL("../examples/local-lv-pong", import.meta.url),
);
const publicRoot = fileURLToPath(new URL("./public", import.meta.url));

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

        const runtimeRoot = `${pongRoot}/priv/static/assets/js`;
        await mkdir(`${publicRoot}/_astro`, { recursive: true });
        await mkdir(`${publicRoot}/wasm`, { recursive: true });
        await Promise.all([
          copyFile(
            `${runtimeRoot}/AtomVM.mjs`,
            `${publicRoot}/_astro/AtomVM.mjs`,
          ),
          copyFile(
            `${runtimeRoot}/AtomVM.wasm`,
            `${publicRoot}/_astro/AtomVM.wasm`,
          ),
          copyFile(
            `${runtimeRoot}/iframe.mjs`,
            `${publicRoot}/_astro/iframe.mjs`,
          ),
          copyFile(
            `${runtimeRoot}/wasm/bundle.avm`,
            `${publicRoot}/wasm/local_pong.avm`,
          ),
        ]);
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
  integrations: [compileGameOfLife(), compilePong()],
});
