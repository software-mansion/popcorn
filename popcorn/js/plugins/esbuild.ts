import { copyFile, mkdir } from "fs/promises";
import { dirname, join, basename, resolve } from "path";
import { fileURLToPath } from "url";
import type { Plugin } from "esbuild";
import { type PopcornPluginOptions } from "./shared";

const __dirname = dirname(fileURLToPath(import.meta.url));
// Plugin is at dist/plugins/esbuild.mjs, dist/ is one level up
const popcornDistDir = resolve(__dirname, "..");

export function popcorn(options: PopcornPluginOptions): Plugin {
  const bundlePath = options.bundlePath;
  const bundleName = basename(bundlePath);
  const bundleDir = dirname(bundlePath);

  let outputDir: string;
  return {
    name: "popcorn",

    setup(build) {
      build.onStart(() => {
        const opts = build.initialOptions;
        const isEsm = opts.format === "esm";
        const outdirFallback =
          opts.outfile !== undefined ? dirname(opts.outfile) : undefined;
        const outdir = opts.outdir ?? outdirFallback;

        if (!isEsm) {
          throw new Error("[popcorn] Popcorn works only with esm type builds.");
        }
        if (outdir === undefined) {
          throw new Error(
            "[popcorn] outdir is not specified, cannot copy files",
          );
        }

        outputDir = outdir;
      });

      build.onEnd(async () => {
        await mkdir(outputDir, { recursive: true });

        try {
          await Promise.all([
            // Copy bundle to wasm directory
            copy(bundleName, { inDir: bundleDir, outDir: outputDir }),
            // Copy popcorn runtime files to output directory
            // These need to be alongside the bundled code for import.meta.url to work
            copy("iframe.mjs", { inDir: popcornDistDir, outDir: outputDir }),
            copy("AtomVM.mjs", { inDir: popcornDistDir, outDir: outputDir }),
            copy("AtomVM.wasm", { inDir: popcornDistDir, outDir: outputDir }),
          ]);
        } catch (err) {
          throw new Error("[popcorn] Failed to copy files", { cause: err });
        }
      });
    },
  };
}

type InOutDirs = { inDir: string; outDir: string };
async function copy(name: string, { inDir, outDir }: InOutDirs): Promise<void> {
  return copyFile(join(inDir, name), join(outDir, name));
}
