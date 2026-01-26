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

/** Messages sent from iframe to parent window */
export type IframeResponse =
  | { type: "popcorn-init"; value: null }
  | { type: "popcorn-startVm"; value: string }
  | { type: "popcorn-call"; value: CallResponse }
  | { type: "popcorn-callAck"; value: CallAck }
  | { type: "popcorn-stdout"; value: string }
  | { type: "popcorn-stderr"; value: string }
  | { type: "popcorn-heartbeat"; value: null }
  | { type: "popcorn-reload"; value: string | null };
