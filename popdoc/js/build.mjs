import * as esbuild from "esbuild";
import { mkdir, readFile, writeFile } from "fs/promises";
import { createRequire } from "module";
import { popcorn } from "@swmansion/popcorn/esbuild";
import { dirname, resolve } from "path";
import { fileURLToPath } from "url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const require = createRequire(import.meta.url);
const rootDir = resolve(__dirname, "..");
const assetsDir = resolve(rootDir, "assets");
const bundlePath = resolve(rootDir, "wasm/out/bundle.avm");
const xtermCssSrc = require.resolve("@xterm/xterm/css/xterm.css");

await mkdir(assetsDir, { recursive: true });

await esbuild.build({
  entryPoints: [resolve(__dirname, "src/popdoc.js")],
  bundle: true,
  format: "esm",
  outfile: resolve(assetsDir, "popdoc.js"),
  plugins: [popcorn({ bundlePaths: [bundlePath] })],
});

const [xtermCss, popdocCss] = await Promise.all([
  readFile(xtermCssSrc, "utf8"),
  readFile(resolve(__dirname, "src/popdoc.css"), "utf8"),
]);

await writeFile(
  resolve(assetsDir, "popdoc.css"),
  `${xtermCss}\n${popdocCss}`,
);

console.log("[popdoc] runtime scaffold built into assets/");
