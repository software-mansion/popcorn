/** Error codes for recoverable errors returned in CallResult */
export type PopcornErrorCode = "timeout" | "deinitialized" | "reload";

const defaultErrorMessages: Record<PopcornErrorCode, string> = {
  timeout: "Promise timeout",
  deinitialized: "Call cancelled due to instance deinit",
  reload: "Call cancelled due to iframe reload",
};

/** Recoverable error returned in CallResult (never thrown) */
export class PopcornError extends Error {
  constructor(
    public readonly code: PopcornErrorCode,
    message?: string,
  ) {
    super(message ?? defaultErrorMessages[code]);
    this.name = "PopcornError";
  }
}

/** Error codes for internal errors that indicate bugs or misuse */
export type PopcornInternalErrorCode =
  | "assert"
  | "private_constructor"
  | "bad_call"
  | "no_acked_call"
  | "bad_ack"
  | "already_mounted"
  | "unmounted"
  | "bad_target"
  | "bad_status"
  | "app_ready_timeout"
  | "bundle_not_found";

const INIT_VM_TIMEOUT_MS = 30_000;

/** Non-recoverable error indicating a bug or library misuse (always thrown) */
export class PopcornInternalError extends Error {
  constructor(
    public readonly code: PopcornInternalErrorCode,
    message?: string,
  ) {
    super(message ?? `Internal error: ${code}`);
    this.name = "PopcornInternalError";
  }
}

/** Internal errors - indicate bugs, protocol violations, or library misuse */
type ErrorData =
  | { t: "assert" }
  | { t: "private_constructor" }
  | { t: "bad_call" }
  | { t: "no_acked_call" }
  | { t: "bad_ack" }
  | { t: "already_mounted" }
  | { t: "unmounted" }
  | { t: "bad_target" }
  | {
      t: "bad_status";
      status: string;
      expectedStatus: string;
    }
  | { t: "app_ready_timeout" }
  | { t: "bundle_not_found"; primary: string; fallback: string };

export function buildError(error: ErrorData): PopcornInternalError {
  switch (error.t) {
    case "assert":
      return new PopcornInternalError("assert", "Assertion error");
    case "private_constructor":
      return new PopcornInternalError(
        "private_constructor",
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    case "bad_call":
      return new PopcornInternalError(
        "bad_call",
        "Response for non-existent call",
      );
    case "no_acked_call":
      return new PopcornInternalError(
        "no_acked_call",
        "Response for non-acknowledged call",
      );
    case "bad_ack":
      return new PopcornInternalError("bad_ack", "Ack for non-existent call");
    case "already_mounted":
      return new PopcornInternalError(
        "already_mounted",
        "Iframe already mounted",
      );
    case "unmounted":
      return new PopcornInternalError("unmounted", "WASM iframe not mounted");
    case "bad_target":
      return new PopcornInternalError(
        "bad_target",
        "Unspecified target process",
      );
    case "bad_status":
      return new PopcornInternalError(
        "bad_status",
        `Operation not allowed: instance in "${error.status}" state, expected "${error.expectedStatus}"`,
      );
    case "app_ready_timeout":
      return new PopcornInternalError(
        "app_ready_timeout",
        `Elixir app did not call Popcorn.Wasm.ready() within ${INIT_VM_TIMEOUT_MS}ms`,
      );
    case "bundle_not_found":
      return new PopcornInternalError(
        "bundle_not_found",
        `Could not find a valid .avm bundle at "${error.primary}" or fallback "${error.fallback}"`,
      );
  }
}

export function throwError(error: ErrorData): never {
  throw buildError(error);
}
