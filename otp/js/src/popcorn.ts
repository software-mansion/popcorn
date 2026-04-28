import {
  PopcornError,
  buildPopcornError,
  deserializePopcornError,
} from "./errors";
import { toVm } from "./events";
import type { BeamBootOptions } from "./types";
import { check, objectWithKeys, unreachable } from "./utils";

export type PopcornOpts = {
  beam: Pick<BeamBootOptions, "assetsUrl">;
  timeoutsMs?: {
    boot?: number;
  };
};

export type PopcornInitResult =
  | { ok: true; popcorn: Popcorn }
  | { ok: false; error: PopcornError };

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
};

export class Popcorn {
  private vmWorker: Worker;

  private constructor(vmWorker: Worker) {
    this.vmWorker = vmWorker;
  }

  public static async init(opts: PopcornOpts): Promise<PopcornInitResult> {
    const vmWorkerUrl = new URL("./worker.mjs", import.meta.url);
    const vmWorker = new Worker(vmWorkerUrl, { type: "module" });

    return await new Promise<PopcornInitResult>((resolve) => {
      let isSettled = false;
      const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...opts.timeoutsMs };
      const cleanup = () => {
        vmWorker.removeEventListener("message", onMessage);
        vmWorker.removeEventListener("error", onError);
      };
      const settle = (result: PopcornInitResult) => {
        if (isSettled) return;
        isSettled = true;
        clearTimeout(timer);
        cleanup();
        if (!result.ok) {
          vmWorker.terminate();
        }
        resolve(result);
      };

      const timer = setTimeout(() => {
        settle({
          ok: false,
          error: buildPopcornError({
            t: "boot-timeout",
            timeoutMs: timeoutsMs.boot,
          }),
        });
      }, timeoutsMs.boot);

      const onError = (event: ErrorEvent) => {
        settle({
          ok: false,
          error: buildPopcornError({
            t: "worker-load",
            message: event.message ?? "Worker failed to load",
          }),
        });
      };
      const onMessage = (event: MessageEvent<unknown>) => {
        const data = objectWithKeys(event.data, ["type", "payload"]);
        check(data !== null);

        switch (data.type) {
          case "popcorn:boot-end":
            settle({ ok: true, popcorn: new Popcorn(vmWorker) });
            break;
          case "popcorn:boot-fail": {
            settle({
              ok: false,
              error: deserializePopcornError(data.payload),
            });
            break;
          }
          default:
            unreachable();
        }
      };

      vmWorker.addEventListener("message", onMessage);
      vmWorker.addEventListener("error", onError);
      toVm(vmWorker, { type: "popcorn:boot", payload: opts.beam });
    });
  }

  public deinit(): void {
    this.vmWorker.terminate();
  }
}
