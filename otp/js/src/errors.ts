export type Result<T, E extends Tag = Tag> =
  | { ok: true; data: T }
  | { ok: false; error: PopcornError<E> };

type Tag = keyof PopcornErrors;
export type PopcornErrors = {
  "timeout:init": { timeoutMs: number };
  "worker:load": { message: string };
  "bridge:not-started": EmptyData;
  "beam:missing-boot-script": { url: string };
  "beam:missing-manifest": { url: string };
  "beam:missing-tarball": { name: string; all: string[] };
  "internal:check": { detail?: string };
  "internal:unreachable": EmptyData;
};

export type SerializedError<T extends Tag = Tag> = {
  [K in T]: { t: K; data: PopcornErrors[K] };
}[T];

type EmptyData = Record<never, never>;

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
  declare readonly cause: SerializedError<T>;
  private readonly serialized: SerializedError<T>;

  constructor(cause: SerializedError<T>) {
    super(message(cause), { cause });
    this.name = "PopcornError";
    Object.setPrototypeOf(this, new.target.prototype);
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
    case "worker:load":
      return error.data.message;
    case "bridge:not-started":
      return "Bridge did not start";
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
    default:
      return unreachable();
  }
}

function parse(value: unknown): SerializedError {
  check(objectWithKeys(value, ["t", "data"]));
  switch (value.t) {
    case "timeout:init":
      check(isTimeoutInitData(value.data));
      return { t: value.t, data: value.data };
    case "worker:load":
      check(isWorkerLoadData(value.data));
      return { t: value.t, data: value.data };
    case "bridge:not-started":
      check(isEmptyData(value.data));
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
      check(isEmptyData(value.data));
      return { t: value.t, data: value.data };
    default:
      return unreachable();
  }
}

function isTimeoutInitData(
  value: unknown,
): value is PopcornErrors["timeout:init"] {
  return objectWithKeys(value, ["timeoutMs"]) !== null;
}

function isWorkerLoadData(
  value: unknown,
): value is PopcornErrors["worker:load"] {
  return objectWithKeys(value, ["message"]) !== null;
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
