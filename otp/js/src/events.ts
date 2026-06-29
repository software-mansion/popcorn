import { err, type Result, type SerializedError } from "./errors";
import type {
  AnyValue,
  BeamBootOptions,
  BeamEvent,
  BeamSendPayload,
} from "./types";
import { objectWithKeys } from "./utils";

type BootEvent = {
  type: "popcorn:boot";
  payload: Pick<BeamBootOptions, "assetsUrl" | "searchPaths" | "extraArgs">;
};

type SendEvent = {
  type: "popcorn:send";
  payload: SendRequestPayload;
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

export type MainToVmEvent = BootEvent | SendEvent;

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
    };

export function readMainEvent(value: unknown): MainToVmEvent | null {
  const data = objectWithKeys(value, ["type", "payload"]);
  if (data === null || typeof data.type !== "string") {
    return null;
  }

  switch (data.type) {
    case "popcorn:boot":
    case "popcorn:send":
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
    case "popcorn:boot-end":
    case "popcorn:boot-fail":
    case "popcorn:send-end":
      return data as VmToMainEvent;
    default:
      return null;
  }
}

export function serializeSendPayload(
  targetName: string,
  payload: AnyValue,
  meta: AnyValue,
): Result<BeamSendPayload> {
  if (targetName.length === 0) {
    return { ok: false, error: err("bridge:invalid-target", {}) };
  }

  try {
    return {
      ok: true,
      data: {
        targetName,
        payloadJson: serializeJson(payload),
        metaJson: serializeJson(meta),
      },
    };
  } catch {
    const error = err("bridge:unserializable", {});
    return { ok: false, error };
  }
}

export function deserializeBridgeMessage(
  text: string,
):
  | Extract<BeamEvent, { type: "otp:message" | "otp:error" | "otp:run_js" }>
  | null {
  try {
    const parsed = JSON.parse(text) as unknown;
    if (!isBridgeEnvelope(parsed)) return null;

    switch (parsed.type) {
      case "vm_message":
        return { type: "otp:message", payload: parsed.data };
      case "vm_error":
        return {
          type: "otp:error",
          payload: {
            kind: "error",
            data: parsed.data,
          },
        };
      case "run_js":
        return { type: "otp:run_js", payload: { code: parsed.code } };
      default:
        return null;
    }
  } catch {
    return null;
  }
}

/** Usable only from main context. */
export function toVm(worker: Worker, event: MainToVmEvent): void {
  worker.postMessage(event);
}

/** Usable only from webworkers. */
export function toMain(event: VmToMainEvent): void {
  self.postMessage(event);
}

function isBridgeEnvelope(value: unknown): value is BridgeEnvelope {
  const data = objectWithKeys(value, ["type"]);
  return (
    data !== null &&
    (data.type === "vm_message" ||
      data.type === "vm_error" ||
      data.type === "run_js")
  );
}

function serializeJson(value: AnyValue): string {
  const serialized = JSON.stringify(value);
  return serialized === undefined ? "null" : serialized;
}
