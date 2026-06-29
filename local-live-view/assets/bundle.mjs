// Release-time bundling script. Runs in this repo (not user's project).
// Produces:
//   ../priv/static/local_live_view.js   — bundled JS with popcorn deps inlined
//   ../priv/static/iframe.mjs           — popcorn runtime
//   ../priv/static/AtomVM.mjs           — popcorn runtime
//   ../priv/static/AtomVM.wasm          — popcorn runtime
//
// These files ship inside the hex package. `mix llv.build` in user projects
// just copies the runtime files to their priv/static; the bundled JS is
// resolved by Phoenix's esbuild via NODE_PATH (because we ship a package.json
// at the hex package root pointing to priv/static/local_live_view.js).
//
// One-shot build:  npm run build   (cd local-live-view/assets; or `mise run build-llv-js`)
// Watch mode:      npm run dev      (or `mise run dev-llv-js`)

import * as esbuild from "esbuild";
import { copyFile, mkdir } from "fs/promises";
import { dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const out = resolve(__dirname, "../priv/static");
const popcornDist = resolve(__dirname, "node_modules/@swmansion/popcorn/dist");

const watch = process.argv.includes("--watch");

const buildOpts = {
  entryPoints: [resolve(__dirname, "local_live_view.js")],
  bundle: true,
  format: "esm",
  outfile: resolve(out, "local_live_view.js"),
};

await mkdir(out, { recursive: true });

// Runtime files come from @swmansion/popcorn/dist and rarely change, so copy
// them once up front rather than on every rebuild.
await Promise.all([
  copyFile(resolve(popcornDist, "iframe.mjs"), resolve(out, "iframe.mjs")),
  copyFile(resolve(popcornDist, "AtomVM.mjs"), resolve(out, "AtomVM.mjs")),
  copyFile(resolve(popcornDist, "AtomVM.wasm"), resolve(out, "AtomVM.wasm")),
]);

if (watch) {
  const ctx = await esbuild.context(buildOpts);
  await ctx.watch();
  console.log("[llv] Watching assets/local_live_view.js → priv/static/...");
} else {
  await esbuild.build(buildOpts);
  console.log("[llv] Release assets built into priv/static/");
}
