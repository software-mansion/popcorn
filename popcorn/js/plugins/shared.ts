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

export async function popcorn(options: Options): Promise<Prepared> {
  const dir = await mkdtemp(`${tmpdir()}/popcorn-otp-`);
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
