export const INIT_TIMEOUT_MS = 30_000;
export const CALL_TIMEOUT_MS = 60_000;
export const HEARTBEAT_TIMEOUT_MS = 60_000;
export const HEARTBEAT_INTERVAL_MS = 500;
export const MAX_RELOAD_N = 3;

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export type AnySerializable = any;

export type CallRequest = {
  requestId: number;
  process: string;
  args: AnySerializable;
};

export type CastRequest = {
  requestId: number;
  process: string;
  args: AnySerializable;
};

/** Messages sent from parent window to iframe */
export type IframeRequest =
  | { type: "popcorn-call"; value: CallRequest }
  | { type: "popcorn-cast"; value: CastRequest };

export type CallResponse = {
  requestId: number;
  error?: AnySerializable;
  data?: AnySerializable;
};

export type CallAck = {
  requestId: number;
};

export type ElixirEvent = {
  eventName: string;
  payload: AnySerializable;
};

/** Messages sent from iframe to parent window */
export type IframeResponse =
  | { type: "popcorn-call"; value: CallResponse }
  | { type: "popcorn-callAck"; value: CallAck }
  | { type: "popcorn-event"; value: ElixirEvent }
  | { type: "popcorn-stdout"; value: string }
  | { type: "popcorn-stderr"; value: string }
  | { type: "popcorn-heartbeat"; value: null }
  | { type: "popcorn-reload"; value: string | null };

/** Union of all messages (requests and responses) */
export type Message = IframeRequest | IframeResponse;

export const MESSAGES = {
  CALL: "popcorn-call",
  CAST: "popcorn-cast",
  CALL_ACK: "popcorn-callAck",
  EVENT: "popcorn-event",
  STDOUT: "popcorn-stdout",
  STDERR: "popcorn-stderr",
  HEARTBEAT: "popcorn-heartbeat",
  RELOAD: "popcorn-reload",
} as const;

export const EVENT_NAMES = {
  ELIXIR_READY: "popcorn_elixir_ready",
  SET_DEFAULT_RECEIVER: "popcorn_set_default_receiver",
} as const;

const MESSAGES_TYPES = new Set<string>(Object.values(MESSAGES));

export function isMessageType(type: string): type is Message["type"] {
  return MESSAGES_TYPES.has(type);
}
