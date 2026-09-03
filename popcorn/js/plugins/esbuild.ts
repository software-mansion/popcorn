import { cp, mkdir, rm } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import type { Plugin } from "esbuild";
import { copyRuntime, popcorn as prepare, type Options } from "./shared";

/**
 * Copies the worker, VM runtime, and `otp/` assets into the output directory after a successful esbuild build.
 *
 * Requires `format: "esm"` and either `outdir` or `outfile`.
 * If the application bundle is elsewhere, set `PopcornOpts.workerUrl` to the copied `worker.mjs`.
 *
 * @see {@link Options} for application packaging and server requirements.
 */
export function popcorn(options: Options): Plugin {
  let outputDir: string | undefined;

  return {
    name: "popcorn-otp",

    setup(build) {
      build.onStart(() => {
        const opts = build.initialOptions;
        const outdir =
          opts.outdir ??
          (opts.outfile === undefined ? undefined : dirname(opts.outfile));

        assert(
          opts.format === "esm",
          "Popcorn OTP works only with esm builds.",
        );
        assert(
          outdir !== undefined,
          "outdir is not specified, cannot copy files",
        );

        outputDir = resolve(outdir);
      });

      build.onEnd(async (result) => {
        if (result.errors.length > 0) return;

        assert(outputDir !== undefined, "outdir was not resolved");
        const outDir = outputDir;
        const prepared = await prepare(options);

        try {
          await mkdir(outDir, { recursive: true });
          await Promise.all([
            copyRuntime(outDir, prepared.runtimeVariant),
            cp(resolve(prepared.dir, "otp"), resolve(outDir, "otp"), {
              recursive: true,
            }),
          ]);
        } finally {
          await rm(prepared.dir, { recursive: true, force: true });
        }
      });
    },
  };
}

function assert(ok: boolean, message: string): asserts ok {
  if (!ok) {
    throw new Error(`[popcorn-otp] ${message}`);
  }
}
