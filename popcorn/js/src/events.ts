import { type Result, type SerializedError } from "./errors";
import { decodeEnvelope } from "./bridge-format";
import { encode, type Mapper } from "./etf";
import type {
  AnyValue,
  BeamBootOptions,
  BeamEvent,
  BeamSendPayload,
  BeamTarget,
} from "./types";
import { check, objectWithKeys, unreachable } from "./utils";

type BootEvent = {
  type: "popcorn:boot";
  payload: Pick<
    BeamBootOptions,
    | "emulatorArgs"
    | "extraArgs"
    | "env"
    | "maxMemoryBytes"
    | "ttySize"
    | "noshell"
    | "extensionPort"
  > & {
    otpAssetsRoot?: string;
    crossOriginStorage?: boolean;
  };
};

type StdinEvent = {
  type: "popcorn:stdin";
  payload: { chunk: Uint8Array };
};

type TtyResizeEvent = {
  type: "popcorn:tty-resize";
  payload: { columns: number; rows: number };
};

type SendEvent = {
  type: "popcorn:send";
  payload: SendRequestPayload;
};

export type BridgeReplyPayload = {
  message: BeamSendPayload;
};

type BridgeReplyEvent = {
  type: "popcorn:bridge-reply";
  payload: BridgeReplyPayload;
};

export type SendRequestPayload = {
  id: string;
  message: BeamSendPayload;
};

export type SerializedSendResult =
  { ok: true; data: null } | { ok: false; error: SerializedError };

export type SendCompletionPayload = {
  id: string;
  result: SerializedSendResult;
};

type SendEndEvent = {
  type: "popcorn:send-end";
  payload: SendCompletionPayload;
};

type BootEndEvent =
  | { type: "popcorn:boot-vm-ready"; payload: {} }
  | { type: "popcorn:boot-end"; payload: {} }
  | { type: "popcorn:boot-fail"; payload: SerializedError };

export type MainToVmEvent =
  | BootEvent
  | SendEvent
  | BridgeReplyEvent
  | StdinEvent
  | TtyResizeEvent;

/**
 * A decoded BEAM message payload.
 *
 * Includes restored PID handles and tracked JavaScript values.
 */
export type PopcornEvent = AnyValue;

type RuntimeEvent = BeamEvent | SendEndEvent;

export type VmToMainEvent = RuntimeEvent | BootEndEvent;

type BridgeEnvelope =
  | {
      type: "vm_message";
      data: Uint8Array;
    }
  | {
      type: "vm_error";
      data: string;
    }
  | {
      type: "run_js";
      code: string;
      args: Uint8Array;
      reply_to: Uint8Array;
      return: "value" | "ref";
    }
  | {
      type: "await_ready";
      reply_to: Uint8Array;
    };

export function readMainEvent(value: unknown): MainToVmEvent {
  const data = objectWithKeys(value, ["type", "payload"]);
  check(data !== null && typeof data.type === "string");

  switch (data.type) {
    case "popcorn:boot":
    case "popcorn:stdin":
    case "popcorn:tty-resize":
    case "popcorn:send":
    case "popcorn:bridge-reply":
      return data as MainToVmEvent;
    default:
      unreachable();
  }
}

export function readWorkerEvent(value: unknown): VmToMainEvent {
  const data = objectWithKeys(value, ["type", "payload"]);
  check(data !== null && typeof data.type === "string");

  switch (data.type) {
    case "otp:stdout":
    case "otp:stderr":
    case "otp:error":
    case "otp:message":
    case "otp:run_js":
    case "otp:await-ready":
    case "otp:tracked-value-delete":
    case "popcorn:boot-vm-ready":
    case "popcorn:boot-end":
    case "popcorn:boot-fail":
    case "popcorn:send-end":
      return data as VmToMainEvent;
    case "otp:stdin-consumed":
      check(Number(data.payload) > 0);
      return data as VmToMainEvent;
    default:
      unreachable();
  }
}

export function serializeSendPayload(
  target: BeamTarget,
  payload: AnyValue,
  mapper?: Mapper,
): Result<BeamSendPayload, "bridge:unserializable"> {
  if (isNameTarget(target)) {
    check(target.name.length > 0);
  } else {
    check(target.pid.byteLength > 0);
  }

  const etf = encode(payload, mapper);
  if (!etf.ok) return etf;
  return { ok: true, data: { target, etf: etf.data } };
}

function isNameTarget(
  target: BeamTarget,
): target is Extract<BeamTarget, { name: string }> {
  return Object.hasOwn(target, "name");
}

type BridgeEvent = Extract<
  BeamEvent,
  { type: "otp:message" | "otp:error" | "otp:run_js" | "otp:await-ready" }
>;

/**
 * Reads a message from the VM, see `wasm.erl`, except its data or args, which
 * the page decodes.
 */
export function deserializeBridgeMessage(
  message: Uint8Array,
): BridgeEvent | null {
  try {
    const value = decodeEnvelope(message);
    return isBridgeEnvelope(value) ? bridgeEvent(value) : null;
  } catch {
    return null;
  }
}

function bridgeEvent(parsed: BridgeEnvelope): BridgeEvent | null {
  switch (parsed.type) {
    case "vm_message":
      check(parsed.data instanceof Uint8Array);
      return { type: "otp:message", payload: parsed.data };
    case "vm_error":
      return {
        type: "otp:error",
        payload: { kind: "error", data: parsed.data },
      };
    case "run_js":
      check(parsed.args instanceof Uint8Array);
      return {
        type: "otp:run_js",
        payload: {
          code: parsed.code,
          args: parsed.args,
          replyTo: parsed.reply_to,
          return: parsed.return,
        },
      };
    case "await_ready":
      return {
        type: "otp:await-ready",
        payload: { replyTo: parsed.reply_to },
      };
    default:
      return null;
  }
}

/** Usable only from main context. */
export function toVm(
  worker: Worker,
  event: MainToVmEvent,
  transfer?: Transferable[],
): void {
  worker.postMessage(event, transfer ?? []);
}

/** Usable only from webworkers. */
export function toMain(event: VmToMainEvent): void {
  self.postMessage(event, { transfer: getTransferables(event) });
}

function getTransferables(event: VmToMainEvent): Transferable[] {
  switch (event.type) {
    case "otp:stdout":
    case "otp:stderr":
      return [ownBuffer(event.payload)];
    // The message's buffer, which the data or args are a part of
    case "otp:message":
      return [ownBuffer(event.payload)];
    case "otp:run_js":
      return [ownBuffer(event.payload.args)];
    default:
      return [];
  }
}

function ownBuffer(bytes: Uint8Array): ArrayBuffer {
  check(bytes.buffer instanceof ArrayBuffer);
  return bytes.buffer;
}

function isBridgeEnvelope(value: unknown): value is BridgeEnvelope {
  const KNOWN_MESSAGE_TYPES: unknown[] = [
    "vm_message",
    "vm_error",
    "run_js",
    "await_ready",
  ];
  const data = objectWithKeys(value, ["type"]);
  return data !== null && KNOWN_MESSAGE_TYPES.includes(data.type);
}
