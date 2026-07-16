export type Result<T, E extends Tag = Tag> =
  | { ok: true; data: T }
  | { ok: false; error: PopcornError<E> };

type Tag = keyof PopcornErrors;
export type PopcornErrors = {
  "timeout:init": { timeoutMs: number };
  "timeout:send": { timeoutMs: number };
  "worker:load": { message: string };
  "vm:exited": VmExitedData;
  "bridge:not-started": EmptyData;
  "bridge:invalid-target": EmptyData;
  "bridge:unserializable": UnserializableData;
  "bridge:listener-not-found": { targetName: string };
  "beam:missing-boot-script": { url: string };
  "beam:missing-manifest": { url: string };
  "beam:missing-tarball": { name: string; all: string[] };
  "internal:check": { detail?: string };
  "internal:unreachable": EmptyData;
  "runtime:eval-unavailable": EmptyData;
};

export type SerializedError<T extends Tag = Tag> = {
  [K in T]: { t: K; data: PopcornErrors[K] };
}[T];

type EmptyData = Record<never, never>;
export type UnserializableReason =
  | "cyclic-object"
  | "non-plain-object"
  | "lossy-int"
  | "non-finite-float"
  | "unsupported";
type UnserializableData = {
  data: unknown;
  part: unknown;
  reason: UnserializableReason;
};
type VmExitedData =
  | { reason: "deinit" }
  | { reason: "abort"; data: string }
  | { reason: "error"; data: string }
  | { reason: "exit"; data: number };

export function err<T extends Tag>(
  t: T,
  data: PopcornErrors[T],
): PopcornError<T> {
  return new PopcornError({ t, data });
}

export function isErr<T extends Tag = Tag>(
  error: unknown,
  t?: T,
): error is PopcornError<T> {
  const isInstance = error instanceof PopcornError;
  if (!isInstance) return false;
  if (t === undefined) return true;
  if (error.t === t) return true;
  return false;
}

export class PopcornError<T extends Tag = Tag> extends Error {
  override readonly cause: SerializedError<T>;
  private readonly serialized: SerializedError<T>;

  constructor(cause: SerializedError<T>) {
    super(message(cause), { cause });
    this.name = "PopcornError";
    Object.setPrototypeOf(this, new.target.prototype);
    this.cause = cause;
    this.serialized = cause;
  }

  public get t(): T {
    return this.serialized.t;
  }

  public get data(): PopcornErrors[T] {
    return this.serialized.data;
  }

  public serialize(): SerializedError<T> {
    return {
      t: this.serialized.t,
      data: { ...this.serialized.data },
    };
  }

  public static deserialize(value: unknown): PopcornError {
    return new PopcornError(parse(value));
  }
}

function message<T extends Tag>(error: SerializedError<T>): string;
function message(error: SerializedError): string {
  switch (error.t) {
    case "timeout:init":
      return `Init timed out after ${error.data.timeoutMs}ms`;
    case "timeout:send":
      return `Send timed out after ${error.data.timeoutMs}ms`;
    case "worker:load":
      return error.data.message;
    case "vm:exited":
      return "VM exited";
    case "bridge:not-started":
      return "Bridge did not start";
    case "bridge:invalid-target":
      return "Target name must not be empty";
    case "bridge:unserializable":
      return "Message can't be serialized to ETF";
    case "bridge:listener-not-found":
      return `Target listener not found: '${error.data.targetName}'`;
    case "beam:missing-boot-script":
      return `Missing boot script: '${error.data.url}'`;
    case "beam:missing-manifest":
      return `Missing tarball manifest: '${error.data.url}'`;
    case "beam:missing-tarball":
      return `Missing tarball: '${error.data.name}'. Available tarballs: ${error.data.all.join(", ")}`;
    case "internal:check":
      return error.data.detail === undefined
        ? "Check failed"
        : `Check failed: ${error.data.detail}`;
    case "internal:unreachable":
      return "Entered unreachable code";
    case "runtime:eval-unavailable":
      return "JS eval is unavailable; run_js requires a Content-Security-Policy that allows 'unsafe-eval'";
    default:
      unreachable();
  }
}

function parse(value: unknown): SerializedError {
  check(objectWithKeys(value, ["t", "data"]));
  switch (value.t) {
    case "timeout:init":
    case "timeout:send":
      check(isTimeoutData(value.data));
      return { t: value.t, data: value.data };
    case "worker:load":
      check(isWorkerLoadData(value.data));
      return { t: value.t, data: value.data };
    case "vm:exited":
      check(isVmExitedData(value.data));
      return { t: value.t, data: value.data };
    case "bridge:not-started":
      check(isEmptyData(value.data));
      return { t: value.t, data: value.data };
    case "bridge:invalid-target":
      check(isEmptyData(value.data));
      return { t: value.t, data: value.data };
    case "bridge:unserializable":
      check(isUnserializableData(value.data));
      return { t: value.t, data: value.data };
    case "bridge:listener-not-found":
      check(isListenerNotFoundData(value.data));
      return { t: value.t, data: value.data };
    case "beam:missing-boot-script":
    case "beam:missing-manifest":
      check(isUrlData(value.data));
      return { t: value.t, data: value.data };
    case "beam:missing-tarball":
      check(isMissingTarballData(value.data));
      return { t: value.t, data: value.data };
    case "internal:check":
      check(isInternalCheckData(value.data));
      return { t: value.t, data: value.data };
    case "internal:unreachable":
    case "runtime:eval-unavailable":
      check(isEmptyData(value.data));
      return { t: value.t, data: value.data };
    default:
      unreachable();
  }
}

function isTimeoutData(
  value: unknown,
): value is PopcornErrors["timeout:init"] {
  return objectWithKeys(value, ["timeoutMs"]) !== null;
}

function isWorkerLoadData(
  value: unknown,
): value is PopcornErrors["worker:load"] {
  return objectWithKeys(value, ["message"]) !== null;
}

function isVmExitedData(value: unknown): value is PopcornErrors["vm:exited"] {
  return objectWithKeys(value, ["reason"]) !== null;
}

function isListenerNotFoundData(
  value: unknown,
): value is PopcornErrors["bridge:listener-not-found"] {
  return objectWithKeys(value, ["targetName"]) !== null;
}

function isUnserializableData(
  value: unknown,
): value is PopcornErrors["bridge:unserializable"] {
  return (
    objectWithKeys(value, ["data", "part", "reason"]) &&
    isUnserializableReason(value.reason)
  );
}

export function isUnserializableReason(
  value: unknown,
): value is UnserializableReason {
  return (
    value === "cyclic-object" ||
    value === "non-plain-object" ||
    value === "lossy-int" ||
    value === "non-finite-float" ||
    value === "unsupported"
  );
}

function isUrlData(
  value: unknown,
): value is PopcornErrors["beam:missing-boot-script"] {
  return objectWithKeys(value, ["url"]) !== null;
}

function isMissingTarballData(
  value: unknown,
): value is PopcornErrors["beam:missing-tarball"] {
  return objectWithKeys(value, ["name", "all"]) !== null;
}

function isInternalCheckData(
  value: unknown,
): value is PopcornErrors["internal:check"] {
  return objectWithKeys(value, []) !== null;
}

function isEmptyData(value: unknown): value is EmptyData {
  return objectWithKeys(value, []) !== null;
}

function objectWithKeys<K extends string>(
  value: unknown,
  keys: readonly K[],
): value is Record<K, unknown> {
  const isObject = value !== null && typeof value === "object";
  return isObject && keys.every((key) => Object.hasOwn(value, key));
}

function unreachable(): never {
  throw err("internal:unreachable", {});
}

function check(ok: boolean, msg?: string): asserts ok {
  if (!ok) {
    throw err("internal:check", { detail: msg });
  }
}
