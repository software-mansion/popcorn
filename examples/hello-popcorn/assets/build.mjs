import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn/esbuild";
import { copyFile, mkdir } from "fs/promises";

await mkdir("../dist", { recursive: true });
await copyFile("index.html", "../dist/index.html");

await esbuild.build({
  entryPoints: ["index.js"],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: "../dist/index.js",
  plugins: [popcorn({ bundlePath: "../dist/wasm/bundle.avm" })],
});
