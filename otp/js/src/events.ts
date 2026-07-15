import { type Result, type SerializedError } from "./errors";
import { tuple2 } from "./etf";
import type {
  AnyValue,
  BeamBootOptions,
  BeamEvent,
  BeamSendPayload,
  BeamTarget,
} from "./types";
import { base64ToBytes, check, objectWithKeys } from "./utils";

type BootEvent = {
  type: "popcorn:boot";
  payload: Pick<BeamBootOptions, "manifestUrl" | "extraArgs">;
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
  | { ok: true; data: null }
  | { ok: false; error: SerializedError };

export type SendCompletionPayload = {
  id: string;
  result: SerializedSendResult;
};

type SendEndEvent = {
  type: "popcorn:send-end";
  payload: SendCompletionPayload;
};

type BootEndEvent =
  | { type: "popcorn:boot-end"; payload: {} }
  | { type: "popcorn:boot-fail"; payload: SerializedError };

export type MainToVmEvent = BootEvent | SendEvent | RunJsReplyEvent;

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
    };

export function readMainEvent(value: unknown): MainToVmEvent | null {
  const data = objectWithKeys(value, ["type", "payload"]);
  if (data === null || typeof data.type !== "string") {
    return null;
  }

  switch (data.type) {
    case "popcorn:boot":
    case "popcorn:send":
    case "popcorn:run-js-reply":
      return data as MainToVmEvent;
    default:
      return null;
  }
}

export function readWorkerEvent(value: unknown): VmToMainEvent | null {
  const data = objectWithKeys(value, ["type", "payload"]);
  if (data === null || typeof data.type !== "string") {
    return null;
  }

  switch (data.type) {
    case "otp:stdout":
    case "otp:stderr":
    case "otp:error":
    case "otp:message":
    case "otp:run_js":
    case "otp:tracked-value-delete":
    case "popcorn:boot-end":
    case "popcorn:boot-fail":
    case "popcorn:send-end":
      return data as VmToMainEvent;
    default:
      return null;
  }
}

export function serializeSendPayload(
  target: BeamTarget,
  payload: AnyValue,
  meta: AnyValue,
): Result<BeamSendPayload> {
  if (isNameTarget(target)) {
    check(target.name.length > 0);
  } else {
    check(target.pid.byteLength > 0);
  }

  const etf = tuple2(payload, meta);
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
  self.postMessage(event);
}

function isBridgeEnvelope(value: unknown): value is BridgeEnvelope {
  const KNOWN_MESSAGE_TYPES: unknown[] = ["vm_message", "vm_error", "run_js"];
  const data = objectWithKeys(value, ["type"]);
  return data !== null && KNOWN_MESSAGE_TYPES.includes(data.type);
}
