import * as esbuild from "esbuild";
import { copyFile, mkdir } from "fs/promises";
import { dirname, join, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const outDir = resolve(__dirname, "../priv/static");
const popcornDist = resolve(
  __dirname,
  "node_modules/@swmansion/popcorn/dist",
);

await mkdir(outDir, { recursive: true });

await esbuild.build({
  entryPoints: [join(__dirname, "local_live_view.js")],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: join(outDir, "local_live_view.js"),
});

// Copy Popcorn runtime files that are resolved via import.meta.url at runtime
await Promise.all([
  copyFile(join(popcornDist, "iframe.mjs"), join(outDir, "iframe.mjs")),
  copyFile(join(popcornDist, "AtomVM.mjs"), join(outDir, "AtomVM.mjs")),
  copyFile(join(popcornDist, "AtomVM.wasm"), join(outDir, "AtomVM.wasm")),
]);
