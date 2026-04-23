import createModule from "../assets/beam.mjs";
import { extractTar } from "./tar";
import type { EmscriptenModule } from "./types";
import {
  assert,
  check,
  dirname,
  ensureDir,
  fetchBinary,
  fetchJson,
  unreachable,
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

export type BeamEvent =
  | [type: "otp:stdout", payload: string]
  | [type: "otp:stderr", payload: string]
  | [type: "otp:exit", payload: number]
  | [type: "otp:abort", payload: string]
  | [type: "otp:error", payload: string];

export type BeamBootOptions = {
  assetsUrl: string;
  searchPaths?: string[];
  extraArgs?: string[];
};

type BootRet =
  | { ok: true; module: EmscriptenModule }
  | { ok: false; error: Error };

export async function boot({
  assetsUrl,
  searchPaths,
  extraArgs,
}: BeamBootOptions): Promise<BootRet> {
  let initError: Error | null = null;

  const moduleConfig: Partial<EmscriptenModule> = {
    print: (text) => emit("otp:stdout", text),
    printErr: (text) => emit("otp:stderr", text),
    onExit: (code) => emit("otp:exit", code),
    onAbort: (text) => emit("otp:abort", text),
    // TODO: add handler for onBeamMessage
    onError: (text) => emit("otp:error", text),
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
            assert(error instanceof Error);
            initError = error;
            emit("otp:abort", error.message);
          } finally {
            mod.removeRunDependency("fs");
          }
        })();
      },
    ],
  };
  const module = await createModule(moduleConfig);
  if (initError) return { ok: false, error: initError };
  return { ok: true, module };
}

function emit(...[type, data]: BeamEvent): void {
  self.postMessage({ type, data });
}

type BuildArgsArgs = {
  searchPaths?: string[];
  extra?: string[];
};
function buildArgs({ searchPaths, extra }: BuildArgsArgs = {}): string[] {
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
  tar: `/lib/${string}.tar`;
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
  check(bootFile !== null, errorMsg("boot-script", { url: BOOT_URL }));
  module.FS.writeFile(BOOT_PATH, bootFile);

  // tarballs.json
  const MANIFEST_URL = `${assetsUrl}${MANIFEST_PATH}`;
  const manifest = await fetchJson<TarballManifest>(MANIFEST_URL);
  check(manifest !== null, errorMsg("manifest", { url: MANIFEST_URL }));

  const createDir = (dirPath: string) => {
    ensureDir(module.FS, dirPath);
  };
  const createFile = (path: string, content: Uint8Array<ArrayBuffer>) => {
    ensureDir(module.FS, dirname(path));
    module.FS.writeFile(path, content);
  };

  await Promise.all(
    CORE_APPS.map(async (name) => {
      const entry = manifest[name] ?? null;
      check(entry !== null, errorMsg("tar-missing", { manifest, name }));
      const tar = await fetchBinary(`${assetsUrl}${entry.tar}`);
      check(tar !== null, errorMsg("tar-missing", { manifest, name }));
      extractTar(tar, createDir, createFile);
    }),
  );
}

type FsErrors =
  | [type: "manifest", data: { url: string }]
  | [type: "boot-script", data: { url: string }]
  | [type: "tar-missing", data: { manifest: TarballManifest; name: string }];

function errorMsg(...args: FsErrors): string {
  const [type, data] = args;

  switch (type) {
    case "manifest":
      return `Missing tarball manifest: '${data.url}'`;
    case "boot-script":
      return `Missing boot script: '${data.url}'`;
    case "tar-missing": {
      const onlyTars = ([app, _value]: [string, unknown]) => app !== "version";
      const tarballs = Object.entries(data.manifest)
        .filter(onlyTars)
        .join(", ");
      return `Missing tarball: '${data.name}'. Available tarballs: ${tarballs}`;
    }
    default:
      unreachable();
  }
}
