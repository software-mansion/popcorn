import * as esbuild from "esbuild";
import { popcorn } from "local_live_view/esbuild";
import { dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const isWatch = process.argv.includes("--watch");

const ctx = await esbuild.context({
  entryPoints: ["js/app.js"],
  bundle: true,
  target: "es2022",
  format: "esm",
  sourcemap: isWatch ? "inline" : true,
  outdir: "../priv/static/assets/js",
  external: ["/fonts/*", "/images/*"],
  nodePaths: [
    resolve(__dirname, "../deps"),
    process.env.MIX_BUILD_PATH ?? resolve(__dirname, "../_build/dev"),
  ],
  plugins: [
    popcorn({
      bundlePaths: [resolve(__dirname, "../priv/static/assets/js/wasm/bundle.avm")],
    }),
  ],
});

if (isWatch) {
  await ctx.watch();
  console.log("[esbuild] watching...");
} else {
  await ctx.rebuild();
  await ctx.dispose();
}
