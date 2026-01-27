type ErrorData =
  | { t: "assert" }
  | {
      t: "bad_status";
      status: string;
      expectedStatus: string;
    }
  | { t: "private_constructor" }
  | { t: "bad_call" }
  | { t: "no_acked_call" }
  | { t: "bad_ack" }
  | { t: "unmounted" }
  | { t: "bad_target" }
  | {
      t: "already_awaited";
      messageType: string;
      awaitedMessageType: string;
    }
  | { t: "already_mounted" };

export function throwError(error: ErrorData): never {
  switch (error.t) {
    case "assert":
      throw new Error("Assertion error");
    case "bad_status":
      throw new Error(
        `Unexpected status transition. Instance in ${error.status} status, expected ${error.expectedStatus}`,
      );
    case "private_constructor":
      throw new Error(
        "Don't construct the Popcorn object directly, use Popcorn.init() instead",
      );
    case "bad_call":
      throw new Error("Response for non-existent call");
    case "no_acked_call":
      throw new Error("Response for non-acknowledged call");
    case "bad_ack":
      throw new Error("Ack for non-existent call");
    case "unmounted":
      throw new Error("WASM iframe not mounted");
    case "bad_target":
      throw new Error("Unspecified target process");

    case "already_awaited":
      throw new Error(
        `Cannot await message ${error.messageType} when a message ${error.awaitedMessageType} is already awaited on`,
      );
    case "already_mounted":
      throw new Error("Iframe already mounted");
  }
}
