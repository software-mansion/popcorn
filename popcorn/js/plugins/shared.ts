import { execFile } from "node:child_process";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { dirname, resolve } from "node:path";
import { tmpdir } from "node:os";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";

const execFileAsync = promisify(execFile);

type RuntimeVariant = "core" | "crypto";

/**
 * Shared options for the Vite, Rollup, and esbuild plugins.
 *
 * Compile the project before the plugin runs. The plugins invoke
 * `mix popcorn.cook` in the project directory.
 */
export type Options = {
  /** Runtime variant override. */
  runtimeVariant?: RuntimeVariant;
  /** Mix project directory. */
  rootDir: string;
  /** OTP application to start after VM boot. */
  app: string | null;
  /** Additional applications to package with their dependencies. */
  extraApps?: string[];
  /** Adds Brotli tarball variants beside gzip and uncompressed files. Defaults to `true`. */
  brotli?: boolean;
  /** Removes nonessential BEAM chunks. Defaults to `true`. */
  strip?: boolean;
  /** Controls removal of unreachable modules and functions. Defaults to `none`. */
  treeshake?: TreeshakeOptions;
};

export type TreeshakeOptions = {
  mode: "all" | "none";
  preservedApps?: string[];
};

export type Prepared = {
  dir: string;
  runtimeVariant: RuntimeVariant;
  notes: unknown[];
};

type CookedManifest = {
  runtimeVariant: RuntimeVariant;
  notes?: unknown[];
};

export async function popcorn(options: Options): Promise<Prepared> {
  const dir = await mkdtemp(`${tmpdir()}/popcorn-otp-`);
  const args = ["popcorn.cook", "--out-dir", dir];

  if (options.app === null) args.push("--no-app");
  else args.push("--app", options.app);

  for (const app of options.extraApps ?? []) args.push("--extra-app", app);
  if (options.runtimeVariant !== undefined) {
    args.push("--runtime-variant", options.runtimeVariant);
  }
  if (options.brotli ?? true) args.push("--brotli");
  if (!(options.strip ?? true)) args.push("--no-strip");
  if (options.treeshake?.mode === "all") {
    args.push("--treeshake");
    for (const app of options.treeshake.preservedApps ?? []) {
      args.push("--preserved-app", app);
    }
  }

  try {
    await execFileAsync("mix", args, {
      cwd: resolve(options.rootDir),
      env: { ...process.env, MIX_QUIET: "1" },
    });

    const manifestPath = resolve(dir, "otp/manifest.json");
    const manifest = JSON.parse(
      await readFile(manifestPath, "utf8"),
    ) as CookedManifest;
    assert(
      manifest.runtimeVariant === "core" ||
        manifest.runtimeVariant === "crypto",
      "popcorn.cook did not report a runtime variant",
    );

    return {
      dir,
      runtimeVariant: manifest.runtimeVariant,
      notes: manifest.notes ?? [],
    };
  } catch (error) {
    await rm(dir, { recursive: true, force: true });
    throw error;
  }
}

export function runtimeDirectory(variant: RuntimeVariant): string {
  const distDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
  return resolve(distDir, "runtimes", variant);
}

function assert(ok: boolean, message: string): asserts ok {
  if (!ok) throw new Error(`[popcorn-otp] ${message}`);
}
