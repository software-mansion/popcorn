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
  check,
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
const MANIFEST_NAME = "manifest.json";
const ENTRYPOINT_READY_EXPR =
  'wasm:send(#{<<"_popcorn">> => #{<<"t">> => <<"boot_ready">>}})';
const ENTRYPOINT_FAILED_EXPR =
  'wasm:send(#{<<"_popcorn">> => #{<<"t">> => <<"boot_failed">>}})';

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

type BeamState = {
  module: EmscriptenModule | null;
  isVmReady: boolean;
};

export type Beam = {
  boot: Promise<Result<null>>;
  vmReady: Promise<void>;
  send: (message: BeamSendPayload) => Result<null>;
  writeStdin: (chunk: Uint8Array) => void;
  resizeTty: (columns: number, rows: number) => void;
  deliverNetworkEvent: (metadata: string, bytes: Uint8Array) => void;
};

export function start(options: BeamBootOptions): Beam {
  const state: BeamState = { module: null, isVmReady: false };
  const vm = trackVmReady(state);
  return {
    boot: boot(options, state, vm),
    vmReady: vm.vmReady,
    send: (message) => send(state.isVmReady ? state.module : null, message),
    writeStdin: (chunk) => writeStdin(state.module, chunk),
    resizeTty: (columns, rows) => resizeTty(state.module, columns, rows),
    deliverNetworkEvent: (metadata, bytes) =>
      deliverNetworkEvent(state.module, metadata, bytes),
  };
}

async function boot(
  opts: BeamBootOptions,
  state: BeamState,
  vm: ReturnType<typeof trackVmReady>,
): Promise<Result<null>> {
  const {
    otpAssetsRoot,
    emulatorArgs,
    extraArgs,
    env,
    ttySize,
    createModule,
    emit,
  } = opts;
  const loadedFsData = await loadFsData(otpAssetsRoot);
  if (!loadedFsData.ok) {
    return { ok: false, error: loadedFsData.error };
  }

  const fsData = loadedFsData.data;
  const { vmReady, handleVmReady } = vm;
  const { appReady, handleAppReady } = trackAppReady(fsData.entrypoint);

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
      if (handleVmReady(event)) return;
      if (handleAppReady(event)) return;

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
    onVirtualNetworkCommand: (metadata, bytes) =>
      emit({ type: "otp:network-command", payload: { metadata, bytes } }),
    arguments: buildArgs({
      appNames: fsData.appNames,
      entrypoint: fsData.entrypoint,
      emulator: emulatorArgs ?? [],
      extra: extraArgs ?? [],
    }),
    preRun: [
      (mod) => {
        state.module = mod;
      },
      (mod) => {
        Object.assign(mod.ENV, runtimeEnv);
        initFs({ module: mod, fsData });
      },
    ],
  };

  try {
    const ready = Promise.all([vmReady, appReady]);
    const module = await createModule(moduleConfig);
    check(state.module === module);
    await ready;
    return { ok: true, data: null };
  } catch (error) {
    return { ok: false, error: toPopcornError(error) };
  }
}

function deliverNetworkEvent(
  module: EmscriptenModule | null,
  metadata: string,
  bytes: Uint8Array,
): void {
  check(module !== null);
  const metadataBytes = UTF8.encode(metadata);
  module.ccall(
    "wasmNetworkEvent",
    "number",
    ["array", "number", "array", "number"],
    [metadataBytes, metadataBytes.byteLength, bytes, bytes.byteLength],
  );
}

type BeamMessage = NonNullable<ReturnType<typeof deserializeBridgeMessage>>;

function trackVmReady(state: BeamState) {
  let resolve = () => {};
  const vmReady = new Promise<void>((r) => {
    resolve = r;
  });

  const handleVmReady = (event: BeamMessage): boolean => {
    if (!isBridgeMarker(event, "vm_ready")) return false;
    state.isVmReady = true;
    resolve();
    return true;
  };

  return { vmReady, handleVmReady };
}

function trackAppReady(entrypoint: string | null) {
  let resolve = () => {};
  let reject = (_error: PopcornError) => {};

  let appReady = Promise.resolve();
  if (entrypoint !== null) {
    appReady = new Promise<void>((res, rej) => {
      resolve = res;
      reject = rej;
    });
  }

  const handleAppReady = (event: BeamMessage): boolean => {
    if (isBridgeMarker(event, "boot_ready")) {
      resolve();
      return true;
    }
    if (isBridgeMarker(event, "boot_failed")) {
      reject(err("vm:exited", { reason: "exit", data: 1 }));
      return true;
    }
    return false;
  };

  return { appReady, handleAppReady };
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
      `case application:ensure_all_started(${entrypoint}) of {ok, _} -> ${ENTRYPOINT_READY_EXPR}; _ -> ${ENTRYPOINT_FAILED_EXPR}, erlang:halt(1) end.`,
    );
  }

  for (const arg of extra) {
    args.push(arg);
  }

  return args;
}

function isBridgeMarker(
  event: ReturnType<typeof deserializeBridgeMessage>,
  type: "vm_ready" | "boot_ready" | "boot_failed",
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

async function loadFsData(assetsRoot: string): Promise<Result<LoadedFsData>> {
  const manifestUrl = resolveAssetsPath(assetsRoot, MANIFEST_NAME);
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

  const bootUrl = resolveAssetsPath(assetsRoot, manifest.vm.boot);
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
      const tarUrl = resolveAssetsPath(assetsRoot, entry.tar);
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

function resolveAssetsPath(assetsRoot: string, relativePath: string): string {
  check(assetsRoot.endsWith("/"));
  if (relativePath.startsWith("/") || isAbsoluteUrl(relativePath)) {
    return relativePath;
  }
  const url = new URL(relativePath, new URL(assetsRoot, self.location.href));
  if (assetsRoot.startsWith("/")) {
    return url.pathname;
  }
  return url.toString();
}

function isAbsoluteUrl(path: string): boolean {
  return /^[a-zA-Z][a-zA-Z\d+\-.]*:/.test(path);
}

function send(
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

function writeStdin(module: EmscriptenModule | null, chunk: Uint8Array): void {
  check(module !== null);
  const status = module.ccall(
    "popcornStdinEnqueue",
    "number",
    ["array", "number"],
    [chunk, chunk.byteLength],
  );
  check(status === 0);
}

function resizeTty(
  module: EmscriptenModule | null,
  columns: number,
  rows: number,
): void {
  check(module !== null);
  const status = module.ccall(
    "popcornTtyResize",
    "number",
    ["number", "number"],
    [columns, rows],
  );
  check(status === 0);
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
