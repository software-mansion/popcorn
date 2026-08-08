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
    "gzip",
    useBrotli && "brotli",
  ];
  const distDir = p`${dirname(fileURLToPath(import.meta.url))}/..`;
  const preparedDir = await mkdtemp(p`${tmpdir()}/popcorn-otp-`);

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
    ]);

    const report = await withTmp(async (packedDir) => {
      const report = await packTarballs({
        rootDir: resolve(opts.rootDir),
        outDir: packedDir,
        manifestPath: p`${distDir}/assets/manifest.json`,
        app: opts.app,
        strip,
      });

      if (!report.ok) {
        throw new Error(`[popcorn-otp] ${formatPackError(report.error)}`);
      }

      await Promise.all([
        copy(report.manifestPath, p`${preparedDir}/${OTP_DIR}/manifest.json`),
        copy(report.bootPath, p`${preparedDir}/${OTP_DIR}/bin/vm.boot`),
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

type PackTarballsParams = {
  rootDir: string;
  outDir: string;
  manifestPath: string;
  app: string | null;
  strip: boolean;
};
async function packTarballs(opts: PackTarballsParams): Promise<Report> {
  const { rootDir, outDir, manifestPath, app, strip } = opts;
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
    "--manifest-path",
    manifestPath,
  ];

  if (app !== null) {
    packerArgs.push("--entrypoint-app", app);
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

function isMissingDepError(error: unknown): error is MissingDepError {
  return (
    typeof error === "object" &&
    error !== null &&
    (error as { code?: unknown }).code === "missing_dep"
  );
}

function formatPackError(error: unknown): string {
  if (isMissingDepError(error)) {
    const { app, dep, available_apps } = error;
    return [
      `${app} depends on ${dep}, which isn't packable.`,
      `Apps available from your project: ${available_apps.join(", ")}.`,
      `Only these can be listed in applications/extra_applications.`,
    ].join("\n  ");
  }

  return `packaging failed: ${JSON.stringify(error)}`;
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
