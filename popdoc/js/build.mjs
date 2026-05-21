import * as esbuild from "esbuild";
import { copyFile, mkdir } from "fs/promises";
import { popcorn } from "@swmansion/popcorn/esbuild";
import { dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const rootDir = resolve(__dirname, "..");
const assetsDir = resolve(rootDir, "assets");
const bundlePath = resolve(rootDir, "wasm/out/bundle.avm");

await mkdir(assetsDir, { recursive: true });

await esbuild.build({
  entryPoints: [resolve(__dirname, "popdoc.js")],
  bundle: true,
  format: "esm",
  outfile: resolve(assetsDir, "popdoc.js"),
  plugins: [popcorn({ bundlePaths: [bundlePath] })],
});

await copyFile(resolve(__dirname, "popdoc.css"), resolve(assetsDir, "popdoc.css"));

console.log("[popdoc] runtime scaffold built into assets/");
