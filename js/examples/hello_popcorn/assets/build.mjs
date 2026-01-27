import * as esbuild from "esbuild";
import { popcorn } from "@swmansion/popcorn/esbuild";

await esbuild.build({
  entryPoints: ["index.js"],
  bundle: true,
  format: "esm",
  sourcemap: true,
  outfile: "out/index.js",
  plugins: [popcorn({ bundlePath: "../_build/wasm/bundle.avm" })],
});
