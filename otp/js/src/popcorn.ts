import { PopcornError, err, type Result } from "./errors";
import { toVm } from "./events";
import type { BeamBootOptions } from "./types";
import { check, objectWithKeys, unreachable } from "./utils";

export type PopcornOpts = {
  beam: Pick<BeamBootOptions, "assetsUrl">;
  timeoutsMs?: {
    boot?: number;
  };
};

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
};

export class Popcorn {
  private vmWorker: Worker;

  private constructor(vmWorker: Worker) {
    this.vmWorker = vmWorker;
  }

  public static async init(opts: PopcornOpts): Promise<Result<Popcorn>> {
    const vmWorkerUrl = new URL("./worker.mjs", import.meta.url);
    const vmWorker = new Worker(vmWorkerUrl, { type: "module" });

    return await new Promise<Result<Popcorn>>((resolve) => {
      let isSettled = false;
      const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...opts.timeoutsMs };
      const cleanup = () => {
        vmWorker.removeEventListener("message", onMessage);
      };
      const settle = (result: Result<Popcorn>) => {
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
          error: err("timeout:init", { timeoutMs: timeoutsMs.boot }),
        });
      }, timeoutsMs.boot);

      const onMessage = (event: MessageEvent<unknown>) => {
        const data = objectWithKeys(event.data, ["type", "payload"]);
        check(data !== null);

        switch (data.type) {
          case "popcorn:boot-end":
            settle({ ok: true, data: new Popcorn(vmWorker) });
            break;
          case "popcorn:boot-fail": {
            settle({
              ok: false,
              error: PopcornError.deserialize(data.payload),
            });
            break;
          }
          default:
            unreachable();
        }
      };

      vmWorker.addEventListener("message", onMessage);
      toVm(vmWorker, { type: "popcorn:boot", payload: opts.beam });
    });
  }

  public deinit(): void {
    this.vmWorker.terminate();
  }
}
