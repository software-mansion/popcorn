import createModule from "../assets/beam.emu";
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

export async function boot(assetsUrl: string) {
  const moduleConfig: Partial<EmscriptenModule> = {
    // TODO: add handler for onBeamMessage
    print: (text) => emit("vm-stdout", text),
    printErr: (text) => emit("vm-stderr", text),
    onExit: (code) => emit("vm-exit", code),
    onAbort: (text) => emit("vm-abort", text),
    // TODO: pass extra args and search paths
    arguments: buildArgs(),
    ENV: {
      BINDIR: "/bin",
      EMU: "beam",
      HOME: DEFAULT_HOME_DIR,
      USER: DEFAULT_USER,
      LOGNAME: DEFAULT_USER,
    },
    prerun: [
      (mod) => {
        (async () => {
          mod.addRunDependency("fs");
          try {
            await initFs({ module: mod, assetsUrl });
          } catch (error) {
            assert(error instanceof Error);
            emit("vm-abort", error.message);
          } finally {
            mod.removeRunDependency("fs");
          }
        })();
      },
    ],
  };
  const module = await createModule(moduleConfig);
  return module;
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

function emit(type: string, data: unknown) {
  self.postMessage({ type, data });
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
export async function initFs({ module, assetsUrl }: InitFsArgs): Promise<void> {
  for (const dir of FS_DIRS) {
    ensureDir(module.FS, dir);
  }

  // vm.boot
  const BOOT_URL = `${assetsUrl}${BOOT_PATH}`;
  const bootFile = await fetchBinary(BOOT_URL);
  check(bootFile !== null, errorMsg("boot-script", { url: BOOT_URL }));
  module.FS.writeFile(BOOT_PATH, bootFile);

  // tarballs
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
      const only_tars = ([app, _v]: [string, unknown]) => app !== "version";
      const tarballs = Object.entries(data.manifest)
        .filter(only_tars)
        .join(", ");
      return `Missing tarball: '${data.name}'. Available tarballs: ${tarballs}`;
    }
    default:
      unreachable();
  }
}
