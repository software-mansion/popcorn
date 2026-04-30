import { PopcornError, err, isErr, type Result } from "./errors";
import { deserializeBridgeMessage } from "./events";
import { extractTar } from "./tar";
import type { BeamBootOptions, EmscriptenModule } from "./types";
import {
  check,
  decompressGzip,
  dirname,
  ensureDir,
  fetchBinary,
  fetchJson,
  isGzip,
} from "./utils";

const DEFAULT_USER = "web_user";
const DEFAULT_HOME_DIR = "/home/web_user";
const FS_DIRS = ["/bin", "/lib", "/etc", "/tmp", "/home", DEFAULT_HOME_DIR];
const BOOT_NAME = "vm";
const BOOT_PATH = `/bin/${BOOT_NAME}.boot`;
const MANIFEST_PATH = "/lib/tarballs.json";
const BASE_ARGS = [
  "--",
  "-root",
  "/",
  "-bindir",
  "/bin",
  "-progname",
  "erl",
  "-home",
  DEFAULT_HOME_DIR,
];

// must be present to load vm
const CORE_APPS = ["kernel", "stdlib", "compiler"];

export async function boot({
  assetsUrl,
  searchPaths,
  extraArgs,
  createModule,
  emit,
}: BeamBootOptions): Promise<Result<EmscriptenModule>> {
  let initError: PopcornError | null = null;

  const moduleConfig: Partial<EmscriptenModule> = {
    print: (text) => emit({ type: "otp:stdout", payload: text }),
    printErr: (text) => emit({ type: "otp:stderr", payload: text }),
    onExit: (code) => emit({ type: "otp:exit", payload: code }),
    onAbort: (text) => emit({ type: "otp:abort", payload: text }),
    onBeamMessage: (text) => {
      emit({ type: "otp:message", payload: deserializeBridgeMessage(text) });
    },
    onError: (text) => emit({ type: "otp:error", payload: text }),
    arguments: buildArgs({ searchPaths, extra: extraArgs }),
    ENV: {
      BINDIR: "/bin",
      EMU: "beam",
      HOME: DEFAULT_HOME_DIR,
      USER: DEFAULT_USER,
      LOGNAME: DEFAULT_USER,
    },
    preRun: [
      (mod) => {
        void (async () => {
          mod.addRunDependency("fs");
          try {
            await initFs({ module: mod, assetsUrl });
          } catch (error) {
            check(error instanceof PopcornError);
            initError = error;
            emit({ type: "otp:abort", payload: error.message });
          } finally {
            mod.removeRunDependency("fs");
          }
        })();
      },
    ],
  };

  try {
    const module = await createModule(moduleConfig);
    if (initError !== null) return { ok: false, error: initError };
    return { ok: true, data: module };
  } catch (error) {
    check(isErr(error));
    return { ok: false, error };
  }
}

type BuildArgsArgs = {
  searchPaths: string[];
  extra: string[];
};

function buildArgs({ searchPaths, extra }: BuildArgsArgs): string[] {
  const args = [...BASE_ARGS, "-boot", BOOT_NAME];

  if (true) {
    args.push("-noshell");
  }

  for (const app of CORE_APPS) {
    args.push("-pa", `/lib/${app}/ebin`);
  }

  for (const path of searchPaths ?? []) {
    args.push("-pa", path);
  }
  for (const arg of extra ?? []) {
    args.push(arg);
  }

  return args;
}

type TarballManifest = { version: string } & {
  [appName: string]: TarballManifestEntry;
};

type TarballManifestEntry = {
  tar: `/lib/${string}.tar` | `/lib/${string}.tar.gz`;
  sha256: string;
};

type InitFsArgs = {
  module: EmscriptenModule;
  assetsUrl: string;
};

async function initFs({ module, assetsUrl }: InitFsArgs): Promise<void> {
  for (const dir of FS_DIRS) {
    ensureDir(module.FS, dir);
  }

  // vm.boot
  const BOOT_URL = `${assetsUrl}${BOOT_PATH}`;
  const bootFile = await fetchBinary(BOOT_URL);
  if (bootFile === null) {
    throw err("beam:missing-boot-script", { url: BOOT_URL });
  }

  module.FS.writeFile(BOOT_PATH, bootFile);

  // tarballs.json
  const MANIFEST_URL = `${assetsUrl}${MANIFEST_PATH}`;
  const manifest = await fetchJson<TarballManifest>(MANIFEST_URL);
  if (manifest === null) {
    throw err("beam:missing-manifest", { url: MANIFEST_URL });
  }

  const createDir = (dirPath: string) => {
    ensureDir(module.FS, dirPath);
  };
  const createFile = (path: string, content: Uint8Array<ArrayBuffer>) => {
    ensureDir(module.FS, dirname(path));
    module.FS.writeFile(path, content);
  };

  const tarballs = await Promise.all(
    CORE_APPS.map(async (name) => {
      const entry = manifest[name] ?? null;
      if (entry === null) {
        throw err("beam:missing-tarball", { name, all: getTarballs(manifest) });
      }

      const tar = await fetchBinary(`${assetsUrl}${entry.tar}`);
      if (tar === null) {
        throw err("beam:missing-tarball", { name, all: getTarballs(manifest) });
      }

      return maybeDecompressTar(tar);
    }),
  );

  for (const tarball of tarballs) {
    extractTar(tarball, createDir, createFile);
  }
}

function getTarballs(manifest: TarballManifest): string[] {
  return Object.keys(manifest).filter((name) => name !== "version");
}

async function maybeDecompressTar(tar: Uint8Array): Promise<Uint8Array> {
  if (!isGzip(tar)) {
    return tar;
  }

  return decompressGzip(tar);
}

export function send(
  module: EmscriptenModule | null,
  command: string,
): Result<null> {
  if (module === null) {
    return { ok: false, error: err("bridge:not-started", {}) };
  }

  const byteLength = new TextEncoder().encode(command).length;
  module.ccall(
    "sendVmMessage",
    null,
    ["string", "number"],
    [command, byteLength],
  );
  return { ok: true, data: null };
}
