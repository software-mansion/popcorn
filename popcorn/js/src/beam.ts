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
  dirname,
  fetchBinary,
  fetchJson,
  objectWithKeys,
  unreachable,
} from "./utils";

const DEFAULT_USER = "web_user";
const DEFAULT_HOME_DIR = "/home/web_user";
const FS_DIRS = ["/bin", "/lib", "/etc", "/tmp", "/home", DEFAULT_HOME_DIR];
const BOOT_NAME = "vm";
const BOOT_PATH = `/bin/${BOOT_NAME}.boot`;
const ENTRYPOINT_READY_EXPR =
  'wasm:send(#{<<"_popcorn">> => #{<<"t">> => <<"boot_ready">>}}).';

// https://www.erlang.org/doc/apps/erts/inet_cfg.html
const INETRC_PATH = "/etc/inetrc";
// lookup types: `native | file | dns`
// We need `file` lookup to avoid spawning
// /bin/inet_gethost which is not available
const INETRC = "{lookup, [file]}.\n";

const STDOUT_FD = 1;
const UTF8 = new TextEncoder();
const BASE_ARGS = [
  "-root",
  "/",
  "-bindir",
  "/bin",
  "-progname",
  "erl",
  "-home",
  DEFAULT_HOME_DIR,
  "-kernel",
  "start_distribution",
  "false",
];

const CORE_APPS = new Set(["kernel", "stdlib", "compiler"]);

export async function boot({
  manifestUrl,
  emulatorArgs,
  extraArgs,
  env,
  ttySize,
  createModule,
  emit,
  captureModule,
  markVmReady,
}: BeamBootOptions): Promise<Result<EmscriptenModule>> {
  const loadedFsData = await loadFsData(manifestUrl);
  if (!loadedFsData.ok) {
    return { ok: false, error: loadedFsData.error };
  }

  const fsData = loadedFsData.data;
  let resolveVmReady = () => {};
  const vmReady = new Promise<void>((resolve) => {
    resolveVmReady = resolve;
  });
  let resolveEntrypointReady = () => {};
  const entrypointReady =
    fsData.entrypoint === null
      ? Promise.resolve()
      : new Promise<void>((resolve) => {
          resolveEntrypointReady = resolve;
        });
  const runtimeEnv = {
    ...env,
    BINDIR: "/bin",
    EMU: "beam",
    HOME: DEFAULT_HOME_DIR,
    USER: DEFAULT_USER,
    LOGNAME: DEFAULT_USER,
    COLUMNS: String(ttySize.columns),
    LINES: String(ttySize.rows),
    ERL_INETRC: INETRC_PATH,
  };
  const moduleConfig: Partial<EmscriptenModule> = {
    print: (text) => emit({ type: "otp:stdout", payload: UTF8.encode(text) }),
    printErr: (text) =>
      emit({ type: "otp:stderr", payload: UTF8.encode(text) }),
    onExit: (code) =>
      emit({ type: "otp:error", payload: { kind: "exit", data: code } }),
    onAbort: (text) =>
      emit({ type: "otp:error", payload: { kind: "abort", data: text } }),
    onBeamMessage: (text) => {
      const event = deserializeBridgeMessage(text);
      if (event === null) return;
      markVmReady();
      if (isBridgeMarker(event, "vm_ready")) {
        resolveVmReady();
        return;
      }
      if (isBridgeMarker(event, "boot_ready")) {
        resolveEntrypointReady();
        return;
      }
      emit(event);
    },
    onError: (text) =>
      emit({ type: "otp:error", payload: { kind: "error", data: text } }),
    onStdinConsumed: (size) =>
      emit({ type: "otp:stdin-consumed", payload: size }),
    onTrackedValueDelete: (key) =>
      emit({ type: "otp:tracked-value-delete", payload: key }),
    onTtyChunk: (fd, chunk) =>
      emit({
        type: fd === STDOUT_FD ? "otp:stdout" : "otp:stderr",
        payload: chunk,
      }),
    arguments: buildArgs({
      appNames: fsData.appNames,
      entrypoint: fsData.entrypoint,
      emulator: emulatorArgs ?? [],
      extra: extraArgs ?? [],
    }),
    preRun: [
      (mod) => {
        captureModule(mod);
        Object.assign(mod.ENV, runtimeEnv);
        initFs({ module: mod, fsData });
      },
    ],
  };

  try {
    const [module] = await Promise.all([
      createModule(moduleConfig),
      vmReady,
      entrypointReady,
    ]);
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
  appNames: string[];
  entrypoint: string | null;
  emulator: string[];
  extra: string[];
};

function buildArgs({
  appNames,
  entrypoint,
  emulator,
  extra,
}: BuildArgsArgs): string[] {
  const args = [...emulator, "--", ...BASE_ARGS, "-boot", BOOT_NAME];

  if (true) {
    args.push("-noshell");
  }

  for (const app of CORE_APPS) {
    args.push("-pa", `/lib/${app}/ebin`);
  }

  for (const app of appNames) {
    if (CORE_APPS.has(app)) continue;
    args.push("-pa", `/lib/${app}/ebin`);
  }

  if (entrypoint !== null) {
    args.push(
      "-eval",
      `{ok, _} = application:ensure_all_started(${entrypoint}), ${ENTRYPOINT_READY_EXPR}`,
    );
  }

  for (const arg of extra) {
    args.push(arg);
  }

  return args;
}

function isBridgeMarker(
  event: ReturnType<typeof deserializeBridgeMessage>,
  type: "vm_ready" | "boot_ready",
): boolean {
  if (event === null || event.type !== "otp:message") return false;
  const popcorn = objectWithKeys(event.payload, ["_popcorn"])?._popcorn;
  return objectWithKeys(popcorn, ["t"])?.t === type;
}

type BeamManifest = {
  entrypoint: string | null;
  apps: Record<string, BeamManifestApp>;
  vm: {
    boot: string;
  };
};

type BeamManifestApp = {
  tar: string;
};

type LoadedFsData = {
  appNames: string[];
  entrypoint: string | null;
  bootFile: Uint8Array;
  tarballs: Uint8Array[];
};

type InitFsArgs = {
  module: EmscriptenModule;
  fsData: LoadedFsData;
};

async function loadFsData(manifestUrl: string): Promise<Result<LoadedFsData>> {
  const manifest = await fetchJson<BeamManifest>(manifestUrl);
  if (manifest === null) {
    return {
      ok: false,
      error: err("beam:missing-manifest", { url: manifestUrl }),
    };
  }

  const appNames = Object.keys(manifest.apps);
  for (const name of CORE_APPS) {
    if (!Object.hasOwn(manifest.apps, name)) {
      return {
        ok: false,
        error: err("beam:missing-tarball", { name, all: appNames }),
      };
    }
  }

  const bootUrl = resolveManifestPath(manifestUrl, manifest.vm.boot);
  const bootFile = await fetchBinary(bootUrl);
  if (bootFile === null) {
    return {
      ok: false,
      error: err("beam:missing-boot-script", { url: bootUrl }),
    };
  }

  const loadedTarballs = await Promise.all(
    appNames.map(async (name): Promise<Result<Uint8Array>> => {
      const entry = manifest.apps[name];
      const tarUrl = resolveManifestPath(manifestUrl, entry.tar);
      const tar = await fetchBinary(tarUrl);
      if (tar === null) {
        return {
          ok: false,
          error: err("beam:missing-tarball", { name, all: appNames }),
        };
      }

      return { ok: true, data: tar };
    }),
  );

  const tarballs: Uint8Array[] = [];
  for (const tarball of loadedTarballs) {
    if (!tarball.ok) {
      return { ok: false, error: tarball.error };
    }

    tarballs.push(tarball.data);
  }

  return {
    ok: true,
    data: {
      appNames,
      entrypoint: manifest.entrypoint ?? null,
      bootFile,
      tarballs,
    },
  };
}

function initFs({ module, fsData }: InitFsArgs): void {
  const writeFile = (path: string, content: Uint8Array) => {
    module.FS_createDataFile(path, null, content, true, true, true);
  };

  for (const dir of FS_DIRS) {
    module.FS_mkdirTree(dir);
  }

  writeFile(BOOT_PATH, fsData.bootFile);
  writeFile(INETRC_PATH, UTF8.encode(INETRC));

  const createDir = (dirPath: string) => {
    module.FS_mkdirTree(dirPath);
  };
  const createFile = (path: string, content: Uint8Array<ArrayBuffer>) => {
    module.FS_mkdirTree(dirname(path));
    writeFile(path, content);
  };

  for (const tarball of fsData.tarballs) {
    extractTar(tarball, createDir, createFile);
  }
}

function resolveManifestPath(manifestUrl: string, path: string): string {
  if (path.startsWith("/") || isAbsoluteUrl(path)) {
    return path;
  }

  return new URL(path, new URL(manifestUrl, self.location.href)).toString();
}

function isAbsoluteUrl(path: string): boolean {
  return /^[a-zA-Z][a-zA-Z\d+\-.]*:/.test(path);
}

export function send(
  module: EmscriptenModule | null,
  message: BeamSendPayload,
): Result<null> {
  if (module === null) {
    return { ok: false, error: err("bridge:not-started", {}) };
  }

  let target: PreparedTarget;
  if (isNameTarget(message.target)) {
    const targetName = message.target.name;
    target = {
      kind: TARGET_REGISTERED_NAME,
      argType: "string",
      value: targetName,
      length: utf8Length(targetName),
    };
  } else {
    const bytes = message.target.pid;
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
    ["number", target.argType, "number", "array", "number"],
    [
      target.kind,
      target.value,
      target.length,
      message.etf,
      message.etf.byteLength,
    ],
  );

  if (status === 0) {
    return { ok: true, data: null };
  }

  if (status === 1) {
    const t = isNameTarget(message.target) ? message.target.name : "<pid>";
    return {
      ok: false,
      error: err("bridge:listener-not-found", { targetName: t }),
    };
  }
  if (status === 2) {
    return {
      ok: false,
      error: err("bridge:unserializable", {
        data: null,
        part: null,
        reason: "unsupported",
      }),
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
