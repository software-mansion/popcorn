import * as esbuild from "esbuild";
import { atomvm } from "@swmansion/popcorn/atomvm/esbuild";
import { copyFile, mkdir } from "fs/promises";

await mkdir("../dist", { recursive: true });
await Promise.all([
  copyFile("index.html", "../dist/index.html"),
  copyFile("style.css", "../dist/style.css"),
]);

await esbuild.build({
  entryPoints: ["index.js"],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: "../dist/index.js",
  plugins: [atomvm({ bundlePaths: ["../dist/wasm/bundle.avm"] })],
});
