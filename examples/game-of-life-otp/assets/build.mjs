import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn-otp/esbuild";
import { copyFile, mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const assetsDir = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(assetsDir, "..");
const outDir = resolve(rootDir, "dist");

await mkdir(outDir, { recursive: true });
await Promise.all([
  copyFile(resolve(assetsDir, "index.html"), resolve(outDir, "index.html")),
  copyFile(resolve(assetsDir, "style.css"), resolve(outDir, "style.css")),
]);

await esbuild.build({
  entryPoints: [resolve(assetsDir, "index.js")],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: resolve(outDir, "index.js"),
  plugins: [popcorn({ rootDir, app: "game_of_life_otp" })],
});
