import { cp, mkdir, rm } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import type { Plugin } from "rollup";
import { popcorn as prepare, type Options } from "./shared";

export function popcorn(options: Options): Plugin {
  let outputDir: string | undefined;

  return {
    name: "popcorn-otp",

    renderStart(outputOptions) {
      assert(
        outputOptions.format === "es",
        "Popcorn OTP works only with esm builds.",
      );

      let dir = outputOptions.dir;
      if (dir === undefined && outputOptions.file !== undefined) {
        dir = dirname(outputOptions.file);
      }
      assert(dir !== undefined, "output dir is not specified, cannot copy files");

      outputDir = resolve(dir);
    },

    async writeBundle() {
      assert(outputDir !== undefined, "output dir was not resolved");
      const outDir = outputDir;
      const prepared = await prepare(options);

      try {
        await mkdir(outDir, { recursive: true });
        await cp(prepared.dir, outDir, { recursive: true });
      } finally {
        await rm(prepared.dir, { recursive: true, force: true });
      }
    },
  };
}

function assert(ok: boolean, message: string): asserts ok {
  if (!ok) {
    throw new Error(`[popcorn-otp] ${message}`);
  }
}
