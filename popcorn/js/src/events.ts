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
    "manifestUrl" | "emulatorArgs" | "extraArgs" | "env" | "ttySize"
  >;
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

export type RunJsReplyPayload = {
  message: BeamSendPayload;
};

type RunJsReplyEvent = {
  type: "popcorn:run-js-reply";
  payload: RunJsReplyPayload;
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
  | RunJsReplyEvent
  | StdinEvent
  | TtyResizeEvent;

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
    };

export function readMainEvent(value: unknown): MainToVmEvent {
  const data = objectWithKeys(value, ["type", "payload"]);
  check(data !== null && typeof data.type === "string");

  switch (data.type) {
    case "popcorn:boot":
    case "popcorn:stdin":
    case "popcorn:tty-resize":
    case "popcorn:send":
    case "popcorn:run-js-reply":
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

export function deserializeBridgeMessage(
  text: string,
): Extract<
  BeamEvent,
  { type: "otp:message" | "otp:error" | "otp:run_js" }
> | null {
  try {
    const parsed = JSON.parse(text) as unknown;
    if (!isBridgeEnvelope(parsed)) return null;

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
      default:
        return null;
    }
  } catch {
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
  const isTtyEvent = event.type === "otp:stdout" || event.type === "otp:stderr";
  if (!isTtyEvent) return [];
  check(event.payload.buffer instanceof ArrayBuffer);
  return [event.payload.buffer];
}

function isBridgeEnvelope(value: unknown): value is BridgeEnvelope {
  const KNOWN_MESSAGE_TYPES: unknown[] = ["vm_message", "vm_error", "run_js"];
  const data = objectWithKeys(value, ["type"]);
  return data !== null && KNOWN_MESSAGE_TYPES.includes(data.type);
}
