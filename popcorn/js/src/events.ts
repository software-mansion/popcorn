import { type Result, type SerializedError } from "./errors";
import { encode, type Mapper } from "./etf";
import type {
  AnyValue,
  BeamBootOptions,
  BeamEvent,
  BeamSendPayload,
  BeamTarget,
} from "./types";
import { base64ToBytes, check, objectWithKeys, unreachable } from "./utils";

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
      data?: AnyValue;
    }
  | {
      type: "vm_error";
      data: string;
    }
  | {
      type: "run_js";
      code: string;
      args: AnyValue;
      reply_to: string;
      return: "value" | "ref";
    }
  | {
      type: "await_ready";
      reply_to: string;
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

const TEXT = new TextDecoder();

// The buffers of binaries attached to events from the VM, which toMain()
// transfers rather than copies
const attachedBuffers = new WeakMap<object, ArrayBuffer[]>();

/**
 * Reads a message from the VM: JSON, or JSON with binaries attached to it,
 * which it refers to by their index. Those messages are bytes: a 0 byte, the
 * JSON and each attachment, after their sizes as 32-bit little-endian
 * integers. See `wasm.erl`.
 */
export function deserializeBridgeMessage(
  message: string | Uint8Array,
): BridgeEvent | null {
  try {
    const { json, attachments } = readMessage(message);
    const parsed = resolveAttachments(JSON.parse(json), attachments);
    if (!isBridgeEnvelope(parsed)) return null;

    const event = bridgeEvent(parsed);
    if (event !== null && attachments.length > 0) {
      attachedBuffers.set(
        event,
        attachments.map((attachment) => attachment.buffer),
      );
    }
    return event;
  } catch {
    return null;
  }
}

function readMessage(message: string | Uint8Array): {
  json: string;
  attachments: Uint8Array<ArrayBuffer>[];
} {
  if (typeof message === "string") return { json: message, attachments: [] };

  check(message[0] === 0);
  const view = new DataView(
    message.buffer,
    message.byteOffset,
    message.byteLength,
  );
  const jsonSize = view.getUint32(1, true);
  const json = TEXT.decode(message.subarray(5, 5 + jsonSize));
  const attachments = [];
  let offset = 5 + jsonSize;
  while (offset < message.byteLength) {
    const size = view.getUint32(offset, true);
    check(offset + 4 + size <= message.byteLength);
    // A copy of its own, as a view of the message would expose the rest of it
    attachments.push(message.slice(offset + 4, offset + 4 + size));
    offset += 4 + size;
  }
  return { json, attachments };
}

// Replaces the markers of attachments with their bytes, or text for strings
function resolveAttachments(
  value: unknown,
  attachments: Uint8Array<ArrayBuffer>[],
): unknown {
  if (attachments.length === 0) return value;
  if (Array.isArray(value)) {
    return value.map((item) => resolveAttachments(item, attachments));
  }
  const object = objectWithKeys(value, []);
  if (object === null) return value;

  const marker = objectWithKeys(value, ["popcorn_value"]);
  if (marker !== null && Object.keys(marker).length === 1) {
    const [tag, index] = marker.popcorn_value as unknown[];
    if (tag === "binary_attachment" || tag === "string_attachment") {
      check(typeof index === "number");
      const attachment = attachments[index];
      check(attachment !== undefined);
      return tag === "binary_attachment" ? attachment : TEXT.decode(attachment);
    }
  }

  const entries = Object.entries(object).map(([key, item]) => [
    key,
    resolveAttachments(item, attachments),
  ]);
  return Object.fromEntries(entries);
}

function bridgeEvent(parsed: BridgeEnvelope): BridgeEvent | null {
  switch (parsed.type) {
    case "vm_message":
      return { type: "otp:message", payload: parsed.data };
    case "vm_error":
      return {
        type: "otp:error",
        payload: { kind: "error", data: parsed.data },
      };
    case "run_js":
      return {
        type: "otp:run_js",
        payload: {
          code: parsed.code,
          args: parsed.args,
          replyTo: base64ToBytes(parsed.reply_to),
          return: parsed.return,
        },
      };
    case "await_ready":
      return {
        type: "otp:await-ready",
        payload: { replyTo: base64ToBytes(parsed.reply_to) },
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
  const attached = attachedBuffers.get(event);
  if (attached !== undefined) return attached;
  const isTtyEvent = event.type === "otp:stdout" || event.type === "otp:stderr";
  if (!isTtyEvent) return [];
  check(event.payload.buffer instanceof ArrayBuffer);
  return [event.payload.buffer];
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
