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
 * The plugins invoke `mix popcorn.cook` in the project directory, which
 * compiles the project before packaging it.
 */
export type Options = {
  /** Runtime variant override. */
  runtimeVariant?: RuntimeVariant;
  /** Mix project directory. */
  rootDir: string;
  /**
   * OTP application to start after VM boot.
   * Defaults to the current Mix application.
   */
  app?: string | null;
  /** Additional applications to package with their dependencies. */
  extraApps?: string[];
  /** Brotli compression effort. Defaults to `standard`. */
  brotliEffort?: "standard" | "max";
  /** Removes nonessential BEAM chunks. Defaults to `true`. */
  strip?: boolean;
  /** Controls removal of unreachable modules and functions. Defaults to `none`. */
  treeshake?: TreeshakeOptions;
};

export type TreeshakeOptions = {
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
  else if (options.app !== undefined) args.push("--app", options.app);

  for (const app of options.extraApps ?? []) args.push("--extra-app", app);
  if (options.runtimeVariant !== undefined) {
    args.push("--runtime-variant", options.runtimeVariant);
  }
  if (options.brotliEffort !== undefined) {
    args.push("--brotli-effort", options.brotliEffort);
  }
  if (options.strip === false) args.push("--no-strip");
  if (options.treeshake !== undefined) {
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
