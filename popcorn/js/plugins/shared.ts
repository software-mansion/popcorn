import { execFile } from "node:child_process";
import { mkdir, mkdtemp, readFile, rm } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
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
  /** Mix project directory. Cooking options belong in its config/config.exs. */
  rootDir: string;
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

/** Cooks the app into a new directory in the bundler's `projectDir`. */
export async function popcorn(
  options: Options,
  projectDir: string,
): Promise<Prepared> {
  // Vite's watcher ignores node_modules, even when it's in the Mix project
  const cacheDir = resolve(projectDir, "node_modules/.cache/popcorn");
  await mkdir(cacheDir, { recursive: true });
  const dir = await mkdtemp(join(cacheDir, "otp-"));
  const args = ["popcorn.cook", "--out-dir", dir];

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
