import assert from "node:assert";
import { execFile } from "node:child_process";
import { copyFile, mkdir, mkdtemp, readdir, rm } from "node:fs/promises";
import { dirname, join, resolve } from "node:path";
import { tmpdir } from "node:os";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";

const execFileAsync = promisify(execFile);
const otpAssetsDirName = "otp-assets";
const manifestUrl = `/${otpAssetsDirName}/manifest.json`;

export type Options = {
  rootDir: string;
  app: string | null;
};

export type Prepared = {
  dir: string;
  manifestUrl: string;
  notes: unknown[];
};

type Report = {
  ok: boolean;
  notes?: unknown[];
  error?: unknown;
};

export async function popcorn(opts: Options): Promise<Prepared> {
  const distDir = resolve(dirname(fileURLToPath(import.meta.url)), "..");
  const distAssetsDir = join(distDir, "assets");
  const preparedDir = await mkdtemp(join(tmpdir(), "popcorn-otp-"));
  const distPath = (name: string) => join(distDir, name);
  const distAssetsPath = (name: string) => join(distAssetsDir, name);
  const preparedPath = (name: string) => join(preparedDir, name);
  const otpAssetsPath = (name: string) =>
    preparedPath(join(otpAssetsDirName, name));
  const providedAppsManifestPath = distAssetsPath("manifest.json");

  try {
    await Promise.all([
      copy(distPath("worker.mjs"), preparedPath("worker.mjs")),
      copy(distAssetsPath("beam.mjs"), preparedPath("assets/beam.mjs")),
      copy(
        distAssetsPath("beam.emu.mjs"),
        preparedPath("assets/beam.emu.mjs"),
      ),
      copy(distAssetsPath("beam.wasm"), preparedPath("assets/beam.wasm")),
      copy(distAssetsPath("bin/vm.boot"), otpAssetsPath("bin/vm.boot")),
      copy(
        distAssetsPath("lib/tarballs.json"),
        otpAssetsPath("lib/tarballs.json"),
      ),
      copyCoreTarballs(distAssetsPath("lib"), otpAssetsPath("lib")),
    ]);

    const report = await packTarballs({
      rootDir: resolve(opts.rootDir),
      outDir: otpAssetsPath(""),
      providedAppsManifestPath,
      app: opts.app,
    });

    assert(
      report.ok,
      `[popcorn-otp] tarballs.exs failed: ${JSON.stringify(report.error)}`,
    );

    return {
      dir: preparedDir,
      manifestUrl,
      notes: report.notes ?? [],
    };
  } catch (error) {
    await rm(preparedDir, { recursive: true, force: true });
    throw error;
  }
}

async function copyCoreTarballs(
  sourceLibDir: string,
  targetLibDir: string,
): Promise<void> {
  const names = await readdir(sourceLibDir);

  await Promise.all(
    names
      .filter((name) => name.endsWith(".tar.gz"))
      .map((name) => copy(join(sourceLibDir, name), join(targetLibDir, name))),
  );
}

async function packTarballs({
  rootDir,
  outDir,
  providedAppsManifestPath,
  app,
}: {
  rootDir: string;
  outDir: string;
  providedAppsManifestPath: string;
  app: string | null;
}): Promise<Report> {
  const scriptPath = join(dirname(fileURLToPath(import.meta.url)), "tarballs.exs");
  const args = [
    scriptPath,
    "--root-dir",
    rootDir,
    "--out-dir",
    outDir,
    "--provided-apps-manifest-path",
    providedAppsManifestPath,
  ];

  if (app !== null) {
    args.push("--entrypoint-app", app);
  }

  const { stdout } = await execFileAsync("elixir", args);
  return JSON.parse(stdout) as Report;
}

async function copy(source: string, target: string): Promise<void> {
  await mkdir(dirname(target), { recursive: true });
  await copyFile(source, target);
}
