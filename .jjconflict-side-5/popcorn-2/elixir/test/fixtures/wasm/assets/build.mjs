import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn/esbuild";
import { copyFile, mkdir } from "fs/promises";

await mkdir("../static", { recursive: true });
await copyFile("index.html", "../static/index.html");

await esbuild.build({
  entryPoints: ["index.js"],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: "../static/index.js",
  plugins: [popcorn({ bundlePaths: [] })],
});
