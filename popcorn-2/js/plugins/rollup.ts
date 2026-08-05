import { readFile } from "fs/promises";
import { dirname, basename, resolve } from "path";
import { fileURLToPath } from "url";
import type { Plugin } from "rollup";
import { type PopcornPluginOptions } from "./shared";

const __dirname = dirname(fileURLToPath(import.meta.url));
// Plugin is at dist/plugins/rollup.mjs, dist/ is one level up
const popcornDistDir = resolve(__dirname, "..");

export function popcorn(options: PopcornPluginOptions): Plugin<unknown> {
  const bundles = options.bundlePaths.map((p) => ({
    path: p,
    name: basename(p),
  }));

  return {
    name: "popcorn",

    async generateBundle() {
      // Emit bundles
      for (const bundle of bundles) {
        this.emitFile({
          type: "asset",
          fileName: resolve(popcornDistDir, bundle.name),
          source: await readFile(bundle.path),
        });
      }

      // Emit popcorn runtime files to output directory
      // These need to be alongside the bundled code for import.meta.url to work
      for (const name of ["iframe.mjs", "AtomVM.mjs", "AtomVM.wasm"]) {
        const sourcePath = resolve(popcornDistDir, name);
        const source = await readFile(sourcePath);

        this.emitFile({ type: "asset", fileName: name, source });
      }
    },
  };
}
