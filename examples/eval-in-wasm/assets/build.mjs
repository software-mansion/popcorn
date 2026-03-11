import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn/esbuild";
import { copyFile, mkdir } from "fs/promises";

await mkdir("../dist", { recursive: true });
await Promise.all([
  copyFile("index.html", "../dist/index.html"),
  copyFile("erlang.html", "../dist/erlang.html"),
  copyFile("style.css", "../dist/style.css"),
  copyFile("favicon.png", "../dist/favicon.png"),
  copyFile("favicon-erl.png", "../dist/favicon-erl.png"),
]);

await esbuild.build({
  entryPoints: ["index.js"],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: "../dist/index.js",
  plugins: [popcorn({ bundlePaths: ["../dist/wasm/bundle.avm"] })],
});
