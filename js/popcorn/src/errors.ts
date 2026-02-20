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
  | "bad_status";

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
    };

export function throwError(error: ErrorData): never {
  switch (error.t) {
    case "assert":
      throw new PopcornInternalError("assert", "Assertion error");
    case "private_constructor":
      throw new PopcornInternalError(
        "private_constructor",
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    case "bad_call":
      throw new PopcornInternalError(
        "bad_call",
        "Response for non-existent call",
      );
    case "no_acked_call":
      throw new PopcornInternalError(
        "no_acked_call",
        "Response for non-acknowledged call",
      );
    case "bad_ack":
      throw new PopcornInternalError("bad_ack", "Ack for non-existent call");
    case "already_mounted":
      throw new PopcornInternalError(
        "already_mounted",
        "Iframe already mounted",
      );
    case "unmounted":
      throw new PopcornInternalError("unmounted", "WASM iframe not mounted");
    case "bad_target":
      throw new PopcornInternalError(
        "bad_target",
        "Unspecified target process",
      );
    case "bad_status":
      throw new PopcornInternalError(
        "bad_status",
        `Operation not allowed: instance in "${error.status}" state, expected "${error.expectedStatus}"`,
      );
  }
}
