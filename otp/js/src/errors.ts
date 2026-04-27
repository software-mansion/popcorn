import { check, objectWithKeys, unreachable } from "./utils";

export type PopcornErrorKind =
  | "boot-failed"
  | "worker-failed"
  | "timeout"
  | "protocol-error";

export type PopcornErrorMetadata = Record<string, unknown>;

export class PopcornError extends Error {
  constructor(
    public readonly kind: PopcornErrorKind,
    message: string,
    public readonly metadata: PopcornErrorMetadata = {},
  ) {
    super(message);
    this.name = "PopcornError";
  }
}

export type SerializedPopcornError = {
  kind: PopcornErrorKind;
  message: string;
  metadata: PopcornErrorMetadata;
};

type PopcornErrorData =
  | { t: "boot-timeout"; timeoutMs: number }
  | { t: "worker-load"; message: string }
  | { t: "protocol"; reason: string; type?: string }
  | { t: "missing-boot-script"; url: string }
  | { t: "missing-manifest"; url: string }
  | { t: "missing-tarball"; name: string; availableTarballs: string[] }
  | { t: "internal-boot-failure" }
  | { t: "internal-worker-failure" };

export function buildPopcornError(data: PopcornErrorData): PopcornError {
  switch (data.t) {
    case "boot-timeout":
      return new PopcornError("timeout", "boot timeout", {
        operation: "boot",
        timeoutMs: data.timeoutMs,
      });
    case "worker-load":
      return new PopcornError("worker-failed", data.message, {
        reason: "load_error",
      });
    case "protocol":
      return new PopcornError(
        "protocol-error",
        data.type === undefined
          ? "Invalid worker event during init"
          : `Unexpected worker event during init: ${data.type}`,
        data.type === undefined
          ? { reason: data.reason }
          : { reason: data.reason, type: data.type },
      );
    case "missing-boot-script":
      return new PopcornError(
        "boot-failed",
        `Missing boot script: '${data.url}'`,
        {
          reason: "missing-boot-script",
          url: data.url,
        },
      );
    case "missing-manifest":
      return new PopcornError(
        "boot-failed",
        `Missing tarball manifest: '${data.url}'`,
        {
          reason: "missing-manifest",
          url: data.url,
        },
      );
    case "missing-tarball":
      return new PopcornError(
        "boot-failed",
        `Missing tarball: '${data.name}'. Available tarballs: ${data.availableTarballs.join(", ")}`,
        {
          reason: "missing-tarball",
          app: data.name,
          availableTarballs: data.availableTarballs,
        },
      );
    case "internal-boot-failure":
      return new PopcornError("boot-failed", "OTP boot failed", {
        reason: "unknown-error",
      });
    case "internal-worker-failure":
      return new PopcornError(
        "worker-failed",
        "Popcorn worker failed during init",
        {
          reason: "internal-error",
        },
      );
  }
}

export function serializePopcornError(
  error: PopcornError,
): SerializedPopcornError {
  return {
    kind: error.kind,
    message: error.message,
    metadata: error.metadata,
  };
}

export function deserializePopcornError(value: unknown): PopcornError {
  const data = objectWithKeys(value, ["kind", "message", "metadata"]);
  check(data !== null);
  check(isMetadata(data.metadata));
  check(typeof data.message === "string");

  return new PopcornError(
    toPopcornErrorKind(data.kind),
    data.message,
    data.metadata,
  );
}

export function toPopcornError(
  error: unknown,
  fallback: PopcornErrorData,
): PopcornError {
  if (error instanceof PopcornError) {
    return error;
  }

  return buildPopcornError(fallback);
}

function toPopcornErrorKind(value: unknown): PopcornErrorKind {
  switch (value) {
    case "boot-failed":
    case "worker-failed":
    case "timeout":
    case "protocol-error":
      return value;
    default:
      unreachable();
  }
}

function isMetadata(value: unknown): value is PopcornErrorMetadata {
  const metadata = objectWithKeys(value, []);
  return metadata !== null;
}
