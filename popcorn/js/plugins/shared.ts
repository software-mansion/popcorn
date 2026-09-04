import { execFile } from "node:child_process";
import {
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { basename, dirname, normalize, resolve } from "node:path";
import { tmpdir } from "node:os";
import { promisify } from "node:util";
import { fileURLToPath } from "node:url";
import { brotliCompress, constants, gzip } from "node:zlib";

const execFileAsync = promisify(execFile);
const brotliCompressAsync = promisify(brotliCompress);
const gzipAsync = promisify(gzip);

/**
 * Shared options for the Vite, Rollup, and esbuild plugins.
 *
 * Compile the project before the plugin runs.
 * The plugins use the local `mix` executable to package applications and dependencies.
 *
 * On the production server, set `Cross-Origin-Opener-Policy: same-origin` and `Cross-Origin-Embedder-Policy: require-corp`.
 * Serve compressed variants with the matching `Content-Encoding` header.
 */
type RuntimeVariant = "core" | "crypto";

export type Options = {
  /**
   * Runtime variant override.
   *
   * By default, the plugin selects `crypto` when an application requires it and `core` otherwise.
   */
  runtimeVariant?: RuntimeVariant;
  /**
   * Mix project directory.
   *
   * Reads compiled apps from `_build/$MIX_ENV/lib`, with `MIX_ENV` defaulting to `dev`.
   */
  rootDir: string;
  /**
   * OTP application to start after VM boot.
   *
   * Use `null` to boot without an entrypoint application.
   */
  app: string | null;
  /**
   * Additional applications to package with their dependencies.
   *
   * Does not start them automatically. Defaults to `[]`.
   */
  extraApps?: string[];
  /**
   * Adds Brotli tarball variants beside the gzip and uncompressed files.
   *
   * Defaults to `false`.
   */
  brotli?: boolean;
  /**
   * Removes nonessential BEAM chunks.
   *
   * Experimental. Defaults to `true`.
   */
  strip?: boolean;
};

export type Prepared = {
  dir: string;
  runtimeVariant: RuntimeVariant;
  notes: unknown[];
};

type Report =
  | {
      ok: true;
      runtimeVariant: RuntimeVariant;
      manifestPath: string;
      bootPath: string;
      tarPaths: string[];
      notes?: unknown[];
    }
  | { ok: false; error: unknown };

type CopyVariant = "gzip" | "brotli" | "uncompressed";
type CopyOptions = {
  variants?: (CopyVariant | false | null | undefined)[];
};

export async function popcorn(opts: Options): Promise<Prepared> {
  const useBrotli = opts.brotli ?? false;
  const strip = opts.strip ?? true;
  const assetVariants: CopyOptions["variants"] = [
    "uncompressed",
    "gzip",
    useBrotli && "brotli",
  ];
  const preparedDir = await mkdtemp(p`${tmpdir()}/popcorn-otp-`);

  try {
    const report = await withTmp(async (packedDir) => {
      const report = await packTarballs({
        rootDir: resolve(opts.rootDir),
        outDir: packedDir,
        runtimeVariant: opts.runtimeVariant,
        app: opts.app,
        extraApps: opts.extraApps ?? [],
        strip,
      });

      if (!report.ok) {
        throw new Error(`[popcorn-otp] ${formatPackError(report.error)}`);
      }

      await Promise.all([
        copy(report.manifestPath, p`${preparedDir}/otp/manifest.json`),
        copy(report.bootPath, p`${preparedDir}/otp/bin/vm.boot`),
        copy(report.tarPaths, p`${preparedDir}/otp/lib`, {
          variants: assetVariants,
        }),
      ]);
      return report;
    });

    return {
      dir: preparedDir,
      runtimeVariant: report.runtimeVariant,
      notes: report.notes ?? [],
    };
  } catch (error) {
    await rm(preparedDir, { recursive: true, force: true });
    throw error;
  }
}

export function runtimeDirectory(variant: RuntimeVariant): string {
  if (variant !== "core" && variant !== "crypto") {
    throw new Error(`[popcorn-otp] Unknown runtime variant: ${variant}`);
  }
  return p`${dirname(fileURLToPath(import.meta.url))}/../runtimes/${variant}`;
}

export async function copyRuntime(
  targetDir: string,
  variant: RuntimeVariant,
): Promise<void> {
  const distDir = p`${dirname(fileURLToPath(import.meta.url))}/..`;
  const runtimeDir = runtimeDirectory(variant);
  await Promise.all(
    ["worker.mjs", "beam.mjs", "beam.emu.mjs", "beam.wasm"].map((file) =>
      copy(
        p`${file === "worker.mjs" ? distDir : runtimeDir}/${file}`,
        p`${targetDir}/${file}`,
      ),
    ),
  );
}

type PackTarballsParams = {
  rootDir: string;
  outDir: string;
  runtimeVariant: Options["runtimeVariant"];
  app: string | null;
  extraApps: string[];
  strip: boolean;
};
async function packTarballs(opts: PackTarballsParams): Promise<Report> {
  const { rootDir, outDir, runtimeVariant, app, extraApps, strip } = opts;
  const toolDir = p`${dirname(fileURLToPath(import.meta.url))}/beam_tools`;

  const packerArgs = [
    "run",
    "--no-start",
    "-e",
    "Popcorn.BeamTools.CLI.main(System.argv())",
    "--",
    "--root-dir",
    rootDir,
    "--out-dir",
    outDir,
    "--runtimes-dir",
    p`${toolDir}/../../runtimes`,
  ];

  if (runtimeVariant !== undefined) {
    packerArgs.push("--runtime-variant", runtimeVariant);
  }

  if (app !== null) {
    packerArgs.push("--entrypoint-app", app);
  }
  for (const extraApp of extraApps) {
    packerArgs.push("--extra-app", extraApp);
  }
  if (strip) {
    packerArgs.push("--strip");
  }

  const env = {
    ...process.env,
    MIX_BUILD_PATH: p`${outDir}/beam_tools_build`,
    MIX_QUIET: "1",
  };
  const { stdout } = await execFileAsync("mix", packerArgs, {
    cwd: toolDir,
    env,
  });
  return JSON.parse(stdout) as Report;
}

type MissingDepError = {
  code: "missing_dep";
  app: string;
  dep: string;
  available_apps: string[];
};

type Toolchain = {
  executable: string;
  otp: string;
  elixir: string;
};

type UnsupportedAppsError = {
  code: "unsupported_apps";
  apps: { app: string; capability: string }[];
};

type MissingExtraAppsError = {
  code: "missing_extra_apps";
  apps: string[];
};

function hasCode(error: unknown, code: string): boolean {
  return (
    typeof error === "object" &&
    error !== null &&
    (error as { code?: unknown }).code === code
  );
}

function isMissingDepError(error: unknown): error is MissingDepError {
  return hasCode(error, "missing_dep");
}

function isUnsupportedAppsError(error: unknown): error is UnsupportedAppsError {
  return hasCode(error, "unsupported_apps");
}

function isMissingExtraAppsError(
  error: unknown,
): error is MissingExtraAppsError {
  return hasCode(error, "missing_extra_apps");
}

function toolchainOf(error: unknown): Toolchain | undefined {
  if (typeof error !== "object" || error === null) {
    return undefined;
  }

  return (error as { toolchain?: Toolchain }).toolchain;
}

function errorLines(error: unknown): string[] {
  if (isMissingDepError(error)) {
    return [
      `${error.app} depends on ${error.dep}, which isn't available.`,
      `BEAM applications come from your project build and your active`,
      `Erlang/Elixir installation; nothing is bundled with the package.`,
      `Apps built by your project: ${error.available_apps.join(", ")}.`,
    ];
  }

  if (isUnsupportedAppsError(error)) {
    const apps = error.apps
      .map(({ app, capability }) => `${app} (needs ${capability})`)
      .join(", ");
    return [
      `These applications need native support the Wasm runtime wasn't built`,
      `with: ${apps}.`,
      `Drop them from your dependencies, or use a runtime built with it.`,
    ];
  }

  if (isMissingExtraAppsError(error)) {
    return [
      `Extra apps not found: ${error.apps.join(", ")}.`,
      `They have to come from your project build or your Erlang/Elixir install.`,
    ];
  }

  return [`packaging failed: ${JSON.stringify(error)}`];
}

function formatPackError(error: unknown): string {
  const lines = errorLines(error);
  const toolchain = toolchainOf(error);
  if (toolchain !== undefined) {
    lines.push(
      `Using ${toolchain.executable} (Erlang/OTP ${toolchain.otp}, Elixir ${toolchain.elixir}).`,
    );
  }

  return lines.join("\n  ");
}

async function copy(
  source: string | string[],
  target: string,
  { variants = ["uncompressed"] }: CopyOptions = {},
): Promise<void> {
  const sources = typeof source === "string" ? [source] : source;
  const targetIsDir = typeof source !== "string";

  await Promise.all(
    sources.map(async (sourcePath) => {
      const targetPath = targetIsDir
        ? p`${target}/${basename(sourcePath)}`
        : target;
      await mkdir(dirname(targetPath), { recursive: true });
      let content: Promise<Buffer> | undefined;
      const read = () => (content ??= readFile(sourcePath));

      await Promise.all(
        variants
          .filter((variant): variant is CopyVariant => Boolean(variant))
          .map(async (variant) => {
            switch (variant) {
              case "uncompressed":
                await copyFile(sourcePath, targetPath);
                break;

              case "gzip": {
                const input = await read();
                const buffer = await gzipAsync(input, { level: 9 });
                await writeFile(`${targetPath}.gz`, buffer);
                break;
              }

              case "brotli": {
                const Q = constants.BROTLI_PARAM_QUALITY;
                const opts = { params: { [Q]: 11 } };

                const input = await read();
                const buffer = await brotliCompressAsync(input, opts);
                await writeFile(`${targetPath}.br`, buffer);
                break;
              }
            }
          }),
      );
    }),
  );
}

function p(
  strings: TemplateStringsArray,
  ...values: (string | number)[]
): string {
  return normalize(String.raw(strings, ...values));
}

async function withTmp<T>(f: (dir: string) => Promise<T>): Promise<T> {
  const dir = await mkdtemp(p`${tmpdir()}/popcorn-otp-`);
  try {
    return await f(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
}
