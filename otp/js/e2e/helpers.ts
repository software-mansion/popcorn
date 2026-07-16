import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import {
  expect,
  test as base,
  type JSHandle,
  type ConsoleMessage,
  type Page,
} from "@playwright/test";
import type {
  Popcorn,
  PopcornOpts,
  PopcornEvent,
  PopcornSendOpts,
  OtpErrorPayload,
  Pid,
  SerializedError,
} from "@swmansion/popcorn-otp";

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    Popcorn: typeof Popcorn;
  }
}

type InitOptions = PopcornOpts;
type BootResult = Result<null>;
type EventWaiter = (event: PopcornEvent) => void;
type OtpFactory = (id?: string) => Promise<OtpHandle>;
type Otp = {
  id: string;
  events: PopcornEvent[];
  boot(options: InitOptions): Promise<BootResult>;
  reboot(): Promise<BootResult>;
  send(
    target: string | Pid,
    payload?: unknown,
    opts?: PopcornSendOpts,
  ): Promise<BootResult>;
  waitForEvent(name: string): Promise<PopcornEvent>;
  deinit(): void;
};

export type Result<T> =
  { ok: true; data: T } | { ok: false; error: SerializedError };

export { assert, expect };

export function evalOpts(code: string): PopcornOpts {
  return {
    beam: {
      manifestUrl: "/assets/otp/manifest.json",
      extraArgs: ["-eval", code],
    },
  };
}

function pidCaptureEval(): PopcornOpts {
  return evalOpts(
    trimLeft(`
      ok = wasm:send(#{pid_captured => self()}),
      receive _ -> ok end.
    `),
  );
}

type PopcornHooksGlobal = typeof globalThis & {
  popcorn: {
    cleanups: number;
    cleanup: () => void;
    runJs: {
      isPaused: () => boolean;
      pause: () => Promise<void>;
      finish: () => void;
    };
  };
};

async function addPopcornHooks(page: Page): Promise<void> {
  await page.evaluate(() => {
    let paused = false;
    let finishRunJs!: () => void;
    const resume = new Promise<void>((resolve) => {
      finishRunJs = resolve;
    });
    const scope = globalThis as PopcornHooksGlobal;
    scope.popcorn = {
      cleanups: 0,
      cleanup() {
        this.cleanups += 1;
      },
      runJs: {
        isPaused: () => paused,
        pause: async () => {
          paused = true;
          await resume;
          paused = false;
        },
        finish: finishRunJs,
      },
    };
  });
}

export async function getCleanups(page: Page): Promise<number> {
  return await page.evaluate(() => {
    return (globalThis as PopcornHooksGlobal).popcorn.cleanups;
  });
}

export async function waitForRunJsSuspension(page: Page): Promise<void> {
  await page.waitForFunction(() => {
    return (globalThis as PopcornHooksGlobal).popcorn.runJs.isPaused();
  });
}

export async function finishRunJs(page: Page): Promise<void> {
  await page.evaluate(() => {
    (globalThis as PopcornHooksGlobal).popcorn.runJs.finish();
  });
}

export function valuesWithKey<K extends PropertyKey>(
  values: Iterable<unknown>,
  key: K,
): unknown[] {
  return Array.from(values)
    .filter((value): value is Record<K, unknown> => {
      return (
        typeof value === "object" &&
        value !== null &&
        Object.hasOwn(value, key)
      );
    })
    .map((value) => value[key]);
}

export const test = base.extend<Fixtures>({
  page: async ({ page }, use) => {
    await page.goto("/");
    await page.waitForFunction(() => window.Popcorn !== undefined);
    await use(page);
  },
  createOtp: async ({ page }, use) => {
    await addPopcornHooks(page);
    const handles = new Set<OtpHandle>();
    const createOtp = async (id = randomOtpId()) => {
      const otp = await OtpHandle.create(page, id);
      handles.add(otp);
      return otp;
    };

    await use(createOtp);
    await Promise.all(Array.from(handles, (otp) => otp.dispose()));
  },
  otp: async ({ createOtp }, use) => {
    const otp = await createOtp();
    await use(otp);
  },
});

export class OtpHandle {
  public readonly events = new Set<PopcornEvent>();
  private otpHandle: JSHandle<Otp> | null;

  private constructor(
    private readonly page: Page,
    public readonly id: string,
    otp: JSHandle<Otp>,
  ) {
    this.otpHandle = otp;
  }

  public static async create(page: Page, id: string): Promise<OtpHandle> {
    const otp = await page.evaluateHandle(createOtp, id);
    return new OtpHandle(page, id, otp);
  }

  public async boot(options: InitOptions): Promise<BootResult> {
    const result = await this.otp.evaluate(
      (otp, initOptions) => otp.boot(initOptions),
      options,
    );
    await this.syncEvents();
    return result;
  }

  public async reboot(): Promise<BootResult> {
    const result = await this.otp.evaluate((otp) => otp.reboot());
    await this.syncEvents();
    return result;
  }

  public async send(
    target: string,
    payload?: unknown,
    opts?: PopcornSendOpts,
  ): Promise<BootResult> {
    const result = await this.otp.evaluate(
      (otp, args) => otp.send(args.target, args.payload, args.opts),
      { target, payload, opts },
    );
    await this.syncEvents();
    return result;
  }

  public async waitForEvent(name: string): Promise<PopcornEvent> {
    const event = await this.otp.evaluate(
      (otp, eventName) => otp.waitForEvent(eventName),
      name,
    );
    await this.syncEvents();
    return event;
  }

  /**
   * Boots the instance and captures its main process as a JS `Pid` (the way the
   * feature delivers one — a `run_js` closure receiving it in args). Returns a
   * handle usable with `sendToPid`.
   */
  public async captureOwnPid(): Promise<JSHandle<Pid>> {
    const boot = await this.boot(pidCaptureEval());
    assert(boot.ok, "pid-capture boot failed");
    await this.waitForEvent("pid_captured");

    return this.otp.evaluateHandle((otp) => {
      const event = otp.events.find(
        (value): value is { pid_captured: Pid } =>
          typeof value === "object" &&
          value !== null &&
          Object.hasOwn(value, "pid_captured"),
      );
      if (event === undefined) throw new Error("no captured pid");
      return event.pid_captured;
    });
  }

  /** Sends to a captured pid through this instance. */
  public async sendToPid(pid: JSHandle<Pid>): Promise<BootResult> {
    const result = await this.otp.evaluate((otp, p) => otp.send(p, {}), pid);
    await this.syncEvents();
    return result;
  }

  public async waitForStderr(text: string): Promise<ConsoleMessage> {
    return await this.page.waitForEvent("console", (message) => {
      return (
        message.type() === "error" &&
        message.text().includes(`[Popcorn-${this.id}]`) &&
        message.text().includes(text)
      );
    });
  }

  public async dispose(): Promise<void> {
    const otp = this.otpHandle;
    this.otpHandle = null;

    if (otp === null) return;
    await otp.evaluate((browserOtp) => browserOtp.deinit());
    await otp.dispose();
  }

  private get otp(): JSHandle<Otp> {
    assert(this.otpHandle !== null, "OTP has been disposed");
    return this.otpHandle;
  }

  private async syncEvents(): Promise<void> {
    const events = await this.otp.evaluate((otp) => otp.events);
    this.events.clear();
    for (const event of events) {
      this.events.add(event);
    }
  }
}

function createOtp(id: string): Otp {
  function logOtpError(logPrefix: string, payload: OtpErrorPayload): void {
    switch (payload.kind) {
      case "abort":
        console.error(`${logPrefix} abort:`, payload.data);
        return;
      case "error":
        console.error(`${logPrefix} error:`, payload.data);
        return;
      case "exit":
        console.info(`${logPrefix} exit:`, payload.data);
        return;
    }
  }

  function hasKey(event: PopcornEvent, key: string) {
    return (
      typeof event === "object" && event !== null && Object.hasOwn(event, key)
    );
  }

  function check(condition: boolean, message: string): asserts condition {
    if (!condition) throw new Error(message);
  }

  class Otp {
    public readonly id = id;
    public readonly events: PopcornEvent[] = [];

    private popcornHandle: Popcorn | null = null;
    private readonly eventWaiters = new Map<string, Array<EventWaiter>>();

    public async boot(options: InitOptions): Promise<BootResult> {
      check(this.popcornHandle === null, "OTP is already booted");

      this.popcornHandle = new window.Popcorn(this.withLogHandlers(options));
      this.popcornHandle.onEvent((event) => {
        this.recordEvent(event);
      });

      const boot = await this.popcornHandle.boot();
      if (boot.ok) return { ok: true, data: null };

      const result: BootResult = {
        ok: false,
        error: boot.error.serialize(),
      };
      this.deinit();
      return result;
    }

    public async reboot(): Promise<BootResult> {
      const popcorn = this.popcorn;
      popcorn.deinit();
      const boot = await popcorn.boot();
      if (boot.ok) return { ok: true, data: null };
      return { ok: false, error: boot.error.serialize() };
    }

    public async send(
      target: string | Pid,
      payload?: unknown,
      opts?: PopcornSendOpts,
    ): Promise<BootResult> {
      const result = await this.popcorn.send(target, payload, opts);
      if (result.ok) return { ok: true, data: null };
      return { ok: false, error: result.error.serialize() };
    }

    public async waitForEvent(name: string): Promise<PopcornEvent> {
      const event = this.findEvent(name);
      if (event !== null) return event;

      return await new Promise<PopcornEvent>((resolve) => {
        const waiters = this.eventWaiters.get(name) ?? [];
        waiters.push(resolve);
        this.eventWaiters.set(name, waiters);
      });
    }

    public deinit(): void {
      const popcorn = this.popcornHandle;
      this.popcornHandle = null;
      popcorn?.deinit();
    }

    private get popcorn() {
      const popcorn = this.popcornHandle;
      check(popcorn !== null, "Popcorn has not been booted");
      return popcorn;
    }

    private get logPrefix(): string {
      return `[Popcorn-${this.id}]`;
    }

    private withLogHandlers(options: InitOptions): InitOptions {
      return {
        ...options,
        onStdout: (text) => console.log(`${this.logPrefix} stdout:`, text),
        onStderr: (text) => console.error(`${this.logPrefix} stderr:`, text),
        onError: (event) => logOtpError(this.logPrefix, event),
      };
    }

    private recordEvent(event: PopcornEvent): void {
      this.events.push(event);

      for (const [name, waiters] of this.eventWaiters) {
        if (hasKey(event, name)) {
          this.eventWaiters.delete(name);
          for (const resolve of waiters) {
            resolve(event);
          }
        }
      }
    }

    private findEvent(name: string): PopcornEvent | null {
      return this.events.find((event) => hasKey(event, name)) ?? null;
    }
  }

  return new Otp();
}

type Fixtures = {
  createOtp: OtpFactory;
  otp: OtpHandle;
};

function randomOtpId(): string {
  return `otp-${randomUUID().slice(0, 8)}`;
}

export function trimLeft(text: string): string {
  const leadingBlanks = /^(?:[ \t]*\n)+/;
  const trailingBlanks = /(?:\n[ \t]*)+$/;
  const trimmedText = text
    .replace(leadingBlanks, "")
    .replace(trailingBlanks, "");
  const lines = trimmedText.split("\n");
  const nonBlank = lines.filter((line) => line.trim() !== "");
  assert(nonBlank.length > 0);

  const indents = nonBlank.map((line) => {
    const trimmedLine = line.trimStart();
    return line.length - trimmedLine.length;
  });
  const indentN = Math.min(...indents);

  const trimmedLines = lines.map((line) => line.slice(indentN));
  return trimmedLines.join("\n");
}
