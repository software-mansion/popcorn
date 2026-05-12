import * as esbuild from "esbuild";
import { copyFile, mkdir } from "fs/promises";
import { dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const projectDir = process.env.LLV_PROJECT_DIR;
if (!projectDir) throw new Error("LLV_PROJECT_DIR is not set");

// local_live_view.js goes to assets/vendor/ so Phoenix esbuild can bundle it
// via a relative import ("../vendor/local_live_view.js") from assets/js/app.js.
const vendorOut = resolve(projectDir, "assets/vendor");

// Runtime files go to priv/static/assets/js/ — same dir where Phoenix esbuild
// outputs app.js. import.meta.url of the bundled code resolves to /assets/js/app.js,
// so ./iframe.mjs etc. are looked up at /assets/js/iframe.mjs.
const staticOut = resolve(projectDir, "priv/static/assets/js");
const popcornDist = resolve(__dirname, "node_modules/@swmansion/popcorn/dist");

await mkdir(vendorOut, { recursive: true });
await mkdir(staticOut, { recursive: true });

await esbuild.build({
  entryPoints: [resolve(__dirname, "local_live_view.js")],
  bundle: true,
  format: "esm",
  outfile: resolve(vendorOut, "local_live_view.js"),
});

await Promise.all([
  copyFile(resolve(popcornDist, "iframe.mjs"), resolve(staticOut, "iframe.mjs")),
  copyFile(resolve(popcornDist, "AtomVM.mjs"), resolve(staticOut, "AtomVM.mjs")),
  copyFile(resolve(popcornDist, "AtomVM.wasm"), resolve(staticOut, "AtomVM.wasm")),
]);

console.log("[llv] JS assets built");
