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
  payload: BeamSendPayload;
};

type BootEndEvent =
  | { type: "popcorn:boot-end"; payload: {} }
  | { type: "popcorn:boot-fail"; payload: SerializedError };

type SendFailEvent = {
  type: "popcorn:send-fail";
  payload: SerializedError;
};

export type MainToVmEvent = BootEvent | SendEvent;

export type PopcornEvent = AnyValue;

type RuntimeEvent = BeamEvent | SendFailEvent;

export type VmToMainEvent = RuntimeEvent | BootEndEvent;

type BridgeEnvelope = {
  type: "vm_message";
  data?: AnyValue;
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
    case "otp:abort":
    case "otp:error":
    case "otp:exit":
    case "otp:message":
    case "popcorn:boot-end":
    case "popcorn:boot-fail":
    case "popcorn:send-fail":
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

export function deserializeBridgeMessage(text: string): AnyValue | null {
  try {
    const parsed = JSON.parse(text) as unknown;
    if (!isBridgeEnvelope(parsed)) return null;
    return parsed.data;
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
  return data !== null && data.type === "vm_message";
}

function serializeJson(value: AnyValue): string {
  const serialized = JSON.stringify(value);
  return serialized === undefined ? "null" : serialized;
}
