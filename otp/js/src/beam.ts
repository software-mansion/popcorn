import { PopcornError, err, isErr, type Result } from "./errors";
import { deserializeBridgeMessage } from "./events";
import { extractTar } from "./tar";
import type {
  BeamBootOptions,
  BeamSendPayload,
  BeamTarget,
  EmscriptenModule,
} from "./types";
import {
  decompressGzip,
  dirname,
  ensureDir,
  fetchBinary,
  fetchJson,
  isGzip,
  unreachable,
} from "./utils";

const DEFAULT_USER = "web_user";
const DEFAULT_HOME_DIR = "/home/web_user";
const FS_DIRS = ["/bin", "/lib", "/etc", "/tmp", "/home", DEFAULT_HOME_DIR];
const BOOT_NAME = "vm";
const BOOT_PATH = `/bin/${BOOT_NAME}.boot`;
const MANIFEST_PATH = "/lib/tarballs.json";
const UTF8 = new TextEncoder();
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
  extraArgs,
  createModule,
  emit,
}: BeamBootOptions): Promise<Result<EmscriptenModule>> {
  let initError: PopcornError | null = null;

  const moduleConfig: Partial<EmscriptenModule> = {
    print: (text) => emit({ type: "otp:stdout", payload: text }),
    printErr: (text) => emit({ type: "otp:stderr", payload: text }),
    onExit: (code) =>
      emit({ type: "otp:error", payload: { kind: "exit", data: code } }),
    onAbort: (text) =>
      emit({ type: "otp:error", payload: { kind: "abort", data: text } }),
    onBeamMessage: (text) => {
      const event = deserializeBridgeMessage(text);
      if (event !== null) {
        emit(event);
      }
    },
    onError: (text) =>
      emit({ type: "otp:error", payload: { kind: "error", data: text } }),
    onTrackedValueDelete: (key) =>
      emit({ type: "otp:tracked-value-delete", payload: key }),
    arguments: buildArgs({
      extra: extraArgs ?? [],
    }),
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
            initError = toPopcornError(error);
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
    return { ok: false, error: toPopcornError(error) };
  }
}

function toPopcornError(error: unknown): PopcornError {
  if (isErr(error)) return error;
  const message = error instanceof Error ? error.message : String(error);
  return err("worker:load", { message });
}

type BuildArgsArgs = {
  extra: string[];
};

function buildArgs({ extra }: BuildArgsArgs): string[] {
  const args = [...BASE_ARGS, "-boot", BOOT_NAME];

  if (true) {
    args.push("-noshell");
  }

  for (const app of CORE_APPS) {
    args.push("-pa", `/lib/${app}/ebin`);
  }

  for (const arg of extra) {
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
  message: BeamSendPayload,
): Result<null> {
  if (module === null) {
    return { ok: false, error: err("bridge:not-started", {}) };
  }

  let targetName: string;
  let target: PreparedTarget;
  if (isNameTarget(message.target)) {
    targetName = message.target.name;
    target = {
      kind: TARGET_REGISTERED_NAME,
      argType: "string",
      value: targetName,
      length: utf8Length(targetName),
    };
  } else {
    targetName = message.target.pid;
    const bytes = base64ToBytes(targetName);
    target = {
      kind: TARGET_PID_BYTES,
      argType: "array",
      value: bytes,
      length: bytes.length,
    };
  }

  const status = module.ccall(
    "sendVmMessage",
    "number",
    [
      "number",
      target.argType,
      "number",
      "string",
      "number",
      "string",
      "number",
    ],
    [
      target.kind,
      target.value,
      target.length,
      message.payloadJson,
      utf8Length(message.payloadJson),
      message.metaJson,
      utf8Length(message.metaJson),
    ],
  );

  if (status === 0) {
    return { ok: true, data: null };
  }

  if (status === 1) {
    return {
      ok: false,
      error: err("bridge:listener-not-found", { targetName }),
    };
  }
  unreachable();
}

const TARGET_REGISTERED_NAME = 0;
const TARGET_PID_BYTES = 1;

type PreparedTarget =
  | {
      kind: typeof TARGET_REGISTERED_NAME;
      argType: "string";
      value: string;
      length: number;
    }
  | {
      kind: typeof TARGET_PID_BYTES;
      argType: "array";
      value: Uint8Array;
      length: number;
    };

function isNameTarget(
  target: BeamTarget,
): target is Extract<BeamTarget, { name: string }> {
  return Object.hasOwn(target, "name");
}

function utf8Length(text: string): number {
  return UTF8.encode(text).length;
}

function base64ToBytes(b64: string): Uint8Array {
  const binary = atob(b64);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) {
    bytes[i] = binary.charCodeAt(i);
  }
  return bytes;
}
