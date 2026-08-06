import { execFile } from "node:child_process";
import {
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  readdir,
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
const OTP_DIR = "assets/otp";

export type Options = {
  rootDir: string;
  app: string | null;
  brotli?: boolean;
  strip?: boolean;
};

export type Prepared = {
  dir: string;
  notes: unknown[];
};

type Report =
  | {
      ok: true;
      manifestPath: string;
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
    "gzip",
    useBrotli && "brotli",
  ];
  const distDir = p`${dirname(fileURLToPath(import.meta.url))}/..`;
  const preparedDir = await mkdtemp(p`${tmpdir()}/popcorn-otp-`);
  const coreTarballs = await pathsIn(p`${distDir}/assets/lib`, ".tar");

  try {
    await Promise.all([
      copy(p`${distDir}/worker.mjs`, p`${preparedDir}/worker.mjs`),
      copy(p`${distDir}/assets/beam.mjs`, p`${preparedDir}/assets/beam.mjs`),
      copy(
        p`${distDir}/assets/beam.emu.mjs`,
        p`${preparedDir}/assets/beam.emu.mjs`,
      ),
      copy(p`${distDir}/assets/beam.wasm`, p`${preparedDir}/assets/beam.wasm`, {
        variants: assetVariants,
      }),
      copy(
        p`${distDir}/assets/bin/vm.boot`,
        p`${preparedDir}/${OTP_DIR}/bin/vm.boot`,
      ),
      copy(
        p`${distDir}/assets/lib/tarballs.json`,
        p`${preparedDir}/${OTP_DIR}/lib/tarballs.json`,
      ),
    ]);

    const report = await withTmp(async (packedDir) => {
      const report = await packTarballs({
        rootDir: resolve(opts.rootDir),
        outDir: packedDir,
        providedAppsManifestPath: p`${distDir}/assets/manifest.json`,
        app: opts.app,
        tarPaths: coreTarballs,
        strip,
      });

      if (!report.ok) {
        throw new Error(`[popcorn-otp] ${formatPackError(report.error)}`);
      }

      await Promise.all([
        copy(report.manifestPath, p`${preparedDir}/${OTP_DIR}/manifest.json`),
        copy(report.tarPaths, p`${preparedDir}/${OTP_DIR}/lib`, {
          variants: assetVariants,
        }),
      ]);
      return report;
    });

    return {
      dir: preparedDir,
      notes: report.notes ?? [],
    };
  } catch (error) {
    await rm(preparedDir, { recursive: true, force: true });
    throw error;
  }
}

async function packTarballs({
  rootDir,
  outDir,
  providedAppsManifestPath,
  app,
  tarPaths,
  strip,
}: {
  rootDir: string;
  outDir: string;
  providedAppsManifestPath: string;
  app: string | null;
  tarPaths: string[];
  strip: boolean;
}): Promise<Report> {
  const scriptPath = p`${dirname(fileURLToPath(import.meta.url))}/tarballs.exs`;
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

  if (strip) {
    args.push("--strip");
  }

  args.push(...tarPaths);

  const { stdout } = await execFileAsync("elixir", args);
  return JSON.parse(stdout) as Report;
}

type MissingDepError = {
  code: "missing_dep";
  app: string;
  dep: string;
  provided_apps: string[];
  user_apps: string[];
};

function isMissingDepError(error: unknown): error is MissingDepError {
  return (
    typeof error === "object" &&
    error !== null &&
    (error as { code?: unknown }).code === "missing_dep"
  );
}

function formatPackError(error: unknown): string {
  if (isMissingDepError(error)) {
    const { app, dep, provided_apps, user_apps } = error;
    return [
      `${app} depends on ${dep}, which isn't packable.`,
      `Apps provided by the runtime: ${provided_apps.join(", ")}.`,
      `Apps in your project: ${user_apps.join(", ")}.`,
      `Only these can be listed in applications/extra_applications.`,
    ].join("\n  ");
  }

  return `tarballs.exs failed: ${JSON.stringify(error)}`;
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

              case "gzip":
                await writeFile(
                  `${targetPath}.gz`,
                  await gzipAsync(await read(), { level: 9 }),
                );
                break;

              case "brotli":
                await writeFile(
                  `${targetPath}.br`,
                  await brotliCompressAsync(await read(), {
                    params: { [constants.BROTLI_PARAM_QUALITY]: 11 },
                  }),
                );
                break;
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

async function pathsIn(dir: string, suffix: string): Promise<string[]> {
  return (await readdir(dir))
    .filter((name) => name.endsWith(suffix))
    .map((name) => p`${dir}/${name}`);
}

async function withTmp<T>(f: (dir: string) => Promise<T>): Promise<T> {
  const dir = await mkdtemp(p`${tmpdir()}/popcorn-otp-`);
  try {
    return await f(dir);
  } finally {
    await rm(dir, { recursive: true, force: true });
  }
}
