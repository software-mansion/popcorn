import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn-otp/esbuild";
import { copyFile, mkdir } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const assetsDir = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(assetsDir, "..");
const outDir = resolve(rootDir, "dist");

await mkdir(outDir, { recursive: true });
await copyFile(resolve(assetsDir, "index.html"), resolve(outDir, "index.html"));

await esbuild.build({
  entryPoints: [resolve(assetsDir, "index.js")],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: resolve(outDir, "index.js"),
  plugins: [popcorn({ rootDir, app: "hello_popcorn_otp" })],
});
