import type { SerializedPopcornError } from "./errors";
import type { BeamBootOptions } from "./types";
import { objectWithKeys } from "./utils";

export type WorkerEvent =
  // events originating from emscripten itself
  | { type: "otp:stdout"; payload: string }
  | { type: "otp:stderr"; payload: string }
  | { type: "otp:exit"; payload: number }
  | { type: "otp:abort"; payload: string }
  | { type: "otp:error"; payload: string }
  // response for `popcorn:boot`
  | { type: "popcorn:boot-end"; payload: null }
  | { type: "popcorn:boot-fail"; payload: SerializedPopcornError };

export type PopcornEvent = {
  type: "popcorn:boot";
  payload: Pick<BeamBootOptions, "assetsUrl">;
};

export function isPopcornEvent(
  rawEvent: MessageEvent<unknown>,
): rawEvent is MessageEvent<PopcornEvent> {
  const data = objectWithKeys(rawEvent.data, ["type", "payload"]);
  return data !== null && data.type === "popcorn:boot";
}

/** Usable only from main context. */
export function toVm(worker: Worker, event: PopcornEvent): void {
  worker.postMessage(event);
}

/** Usable only from webworkers. */
export function toMain(event: WorkerEvent): void {
  self.postMessage(event);
}
