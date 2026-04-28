export type PopcornErrors =
  | { t: "internal:check"; detail?: string }
  | { t: "internal:unreachable" }
  | { t: "timeout:boot"; timeoutMs: number }
  | { t: "worker:load"; message: string }
  | { t: "beam:missing-boot-script"; url: string }
  | { t: "beam:missing-manifest"; url: string }
  | { t: "beam:missing-tarball"; name: string; availableTarballs: string[] };

export type ErrorType = PopcornErrors["t"];
export type ErrorPayloadFor<T extends ErrorType = ErrorType> = Omit<
  Extract<PopcornErrors, { t: T }>,
  "t"
>;
export type ErrorPayload = {
  [T in ErrorType]: ErrorPayloadFor<T>;
}[ErrorType];
export type SerializedError<T extends ErrorType = ErrorType> = Extract<
  PopcornErrors,
  { t: T }
>;

export type AnyPopcornError = PopcornError<any>;

export class PopcornError<T extends ErrorType = ErrorType> extends Error {
  declare readonly cause: SerializedError;
  readonly #serialized: SerializedError<T>;

  constructor(cause: SerializedError<T>) {
    super(message(cause), { cause });
    this.name = "PopcornError";
    Object.setPrototypeOf(this, new.target.prototype);
    this.#serialized = cause;
  }

  public get t(): T {
    return this.#serialized.t;
  }

  public get payload(): ErrorPayloadFor<T> {
    const { t: _t, ...payload } = this.#serialized;
    return payload as ErrorPayloadFor<T>;
  }

  public serialize(): SerializedError<T> {
    return { ...this.#serialized };
  }

  public static deserialize(value: unknown): PopcornError {
    return new PopcornError(parse(value));
  }

  public static fromUnknown<T extends ErrorType>(
    error: unknown,
    fallback: SerializedError<T>,
  ): PopcornError<T> {
    return error instanceof PopcornError ? error : new PopcornError(fallback);
  }
}

function message(error: SerializedError): string {
  switch (error.t) {
    case "internal:check":
      return error.detail === undefined
        ? "Check failed"
        : `Check failed: ${error.detail}`;
    case "internal:unreachable":
      return "Entered unreachable code";
    case "timeout:boot":
      return `Boot timed out after ${error.timeoutMs}ms`;
    case "worker:load":
      return error.message;
    case "beam:missing-boot-script":
      return `Missing boot script: '${error.url}'`;
    case "beam:missing-manifest":
      return `Missing tarball manifest: '${error.url}'`;
    case "beam:missing-tarball":
      return `Missing tarball: '${error.name}'. Available tarballs: ${error.availableTarballs.join(", ")}`;
    default: {
      unreachable();
    }
  }
}

function parse(value: unknown): SerializedError {
  const data = objectWithKeys(value, ["t"]);
  check(data !== null);

  switch (data.t) {
    case "internal:check":
      if (objectWithKeys(value, ["t"]) !== null) {
        return value as SerializedError<"internal:check">;
      }
      break;
    case "internal:unreachable":
      if (objectWithKeys(value, ["t"]) !== null) {
        return value as SerializedError<typeof data.t>;
      }
      break;
    case "timeout:boot":
      if (objectWithKeys(value, ["t", "timeoutMs"]) !== null) {
        return value as SerializedError<"timeout:boot">;
      }
      break;
    case "worker:load":
      if (objectWithKeys(value, ["t", "message"]) !== null) {
        return value as SerializedError<"worker:load">;
      }
      break;
    case "beam:missing-boot-script":
    case "beam:missing-manifest":
      if (objectWithKeys(value, ["t", "url"]) !== null) {
        return value as
          | SerializedError<"beam:missing-boot-script">
          | SerializedError<"beam:missing-manifest">;
      }
      break;
    case "beam:missing-tarball":
      if (objectWithKeys(value, ["t", "name", "availableTarballs"]) !== null) {
        return value as SerializedError<"beam:missing-tarball">;
      }
      break;
  }
  unreachable();
}

// We don't want to create a cycle with utils (which use PopcornError internally).
function unreachable(): never {
  throw new PopcornError({ t: "internal:unreachable" });
}

export function check(ok: boolean, msg?: string): asserts ok {
  if (!ok) throw new PopcornError({ t: "internal:check", detail: msg });
}

function objectWithKeys<K extends string>(
  value: unknown,
  keys: K[],
): null | Record<K, unknown> {
  const isObject = value !== null && typeof value === "object";
  if (!isObject) return null;
  const hasAllKeys = keys.every((k) => Object.hasOwn(value, k));
  if (!hasAllKeys) return null;
  return value as Record<K, unknown>;
}
