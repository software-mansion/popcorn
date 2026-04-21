import { extractTar } from "./tar";
import type { EmscriptenModule } from "./types";
import {
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
const BOOT_PATH = "/bin/vm.boot";
const MANIFEST_PATH = "/lib/tarballs.json";

// must be present to load vm
const CORE_APPS = ["kernel", "stdlib", "compiler"];

type TarballManifestEntry = {
  tar: `/lib/${string}.tar`;
  sha256: string;
};

type InitFsArgs = {
  module: EmscriptenModule;
  assetsUrl: string;
};

export async function initFs({ module, assetsUrl }: InitFsArgs) {
  for (const dir of FS_DIRS) {
    ensureDir(module.FS, dir);
  }

  module.ENV = {
    BINDIR: "/bin",
    EMU: "beam",
    HOME: DEFAULT_HOME_DIR,
    USER: DEFAULT_USER,
    LOGNAME: DEFAULT_USER,
  };

  // vm.boot
  const bootFile = await fetchBinary(`${assetsUrl}${BOOT_PATH}`);
  check(bootFile !== null, errorMsg("boot-script", { assetsUrl }));
  module.FS.writeFile(BOOT_PATH, bootFile);

  // tarballs
  const MANIFEST_URL = `${assetsUrl}${MANIFEST_PATH}`;
  const manifest = await fetchJson<TarballManifest>(MANIFEST_URL);
  check(manifest !== null, errorMsg("manifest", { assetsUrl }));

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

type TarballManifest = { version: string } & {
  [appName: string]: TarballManifestEntry;
};

type FsErrors =
  | [type: "manifest", data: { assetsUrl: string }]
  | [type: "boot-script", data: { assetsUrl: string }]
  | [type: "tar-missing", data: { manifest: TarballManifest; name: string }];
function errorMsg(...args: FsErrors): string {
  const [type, data] = args;
  switch (type) {
    case "manifest":
      return `Missing tarball manifest: '${data.assetsUrl}${MANIFEST_PATH}'`;
    case "boot-script":
      return `Missing boot script: '${data.assetsUrl}${BOOT_PATH}'`;
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
