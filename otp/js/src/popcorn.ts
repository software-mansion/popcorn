import { PopcornError, err, type Result } from "./errors";
import {
  readWorkerEvent,
  serializeSendCommand,
  toVm,
  type PopcornEvent,
} from "./events";
import type { AnyValue, BeamBootOptions } from "./types";
import { check } from "./utils";

export type PopcornOpts = {
  beam: Pick<BeamBootOptions, "assetsUrl">;
  timeoutsMs?: {
    boot?: number;
  };
};

export type PopcornSendOpts = {
  meta?: AnyValue;
};

type ResolvedTimeouts = Required<NonNullable<PopcornOpts["timeoutsMs"]>>;
const DEFAULT_TIMEOUTS_MS: ResolvedTimeouts = {
  boot: 10_000,
};

export class Popcorn {
  private vmWorker: Worker;
  private isBooted = false;
  private readonly eventHandlers = new Set<(event: PopcornEvent) => void>();
  private readonly onWorkerMessage = (event: MessageEvent<unknown>) => {
    const data = readWorkerEvent(event.data);
    check(data !== null);

    switch (data.type) {
      case "popcorn:boot-end":
      case "popcorn:boot-fail":
        return;
      default:
        this.emit(data);
        return;
    }
  };

  private constructor(vmWorker: Worker) {
    this.vmWorker = vmWorker;
  }

  private startEventLoop(): void {
    this.vmWorker.addEventListener("message", this.onWorkerMessage);
  }

  public static async init(opts: PopcornOpts): Promise<Result<Popcorn>> {
    const vmWorkerUrl = new URL("./worker.mjs", import.meta.url);
    const vmWorker = new Worker(vmWorkerUrl, { type: "module" });
    const popcorn = new Popcorn(vmWorker);

    return await new Promise<Result<Popcorn>>((resolve) => {
      let isSettled = false;
      const timeoutsMs = { ...DEFAULT_TIMEOUTS_MS, ...opts.timeoutsMs };
      const cleanup = () => {
        vmWorker.removeEventListener("message", onInitMessage);
      };
      const settle = (result: Result<Popcorn>) => {
        if (isSettled) return;
        isSettled = true;
        clearTimeout(timer);
        cleanup();
        if (!result.ok) {
          popcorn.deinit();
        }
        resolve(result);
      };

      const timer = setTimeout(() => {
        const error = err("timeout:init", { timeoutMs: timeoutsMs.boot });
        settle({ ok: false, error });
      }, timeoutsMs.boot);

      const onInitMessage = (event: MessageEvent<unknown>) => {
        const data = readWorkerEvent(event.data);
        check(data !== null);

        switch (data.type) {
          case "popcorn:boot-end":
            popcorn.isBooted = true;
            popcorn.startEventLoop();
            settle({ ok: true, data: popcorn });
            break;
          case "popcorn:boot-fail": {
            const error = PopcornError.deserialize(data.payload);
            settle({ ok: false, error });
            break;
          }
          default:
            // TODO: decide whether pre-init VM events should be buffered,
            // exposed, or whether init should wait for a dedicated bridge-ready
            // message emitted from BEAM.
            break;
        }
      };

      vmWorker.addEventListener("message", onInitMessage);
      toVm(vmWorker, { type: "popcorn:boot", payload: opts.beam });
    });
  }

  public send(
    target: string,
    payload?: AnyValue,
    opts?: PopcornSendOpts,
  ): Result<null> {
    if (!this.isBooted) {
      return { ok: false, error: err("bridge:not-started", {}) };
    }

    const command = serializeSendCommand(
      { name: target },
      payload ?? {},
      opts?.meta ?? {},
    );
    if (!command.ok) {
      return command;
    }

    toVm(this.vmWorker, {
      type: "popcorn:send",
      payload: { command: command.data },
    });
    return { ok: true, data: null };
  }

  public onEvent(handler: (event: PopcornEvent) => void): () => void {
    this.eventHandlers.add(handler);
    return () => {
      this.eventHandlers.delete(handler);
    };
  }

  public deinit(): void {
    this.isBooted = false;
    this.vmWorker.removeEventListener("message", this.onWorkerMessage);
    this.eventHandlers.clear();
    this.vmWorker.terminate();
  }

  private emit(event: PopcornEvent): void {
    for (const handler of this.eventHandlers) {
      handler(event);
    }
  }
}
