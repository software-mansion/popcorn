import assert from "node:assert/strict";
import { randomUUID } from "node:crypto";
import {
  expect,
  test as base,
  type JSHandle,
  type Page,
} from "@playwright/test";
import type {
  atom,
  Popcorn,
  PopcornOpts,
  PopcornEvent,
  OtpErrorPayload,
  Pid,
  SerializedError,
  tuple,
} from "@swmansion/popcorn-otp";

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    Popcorn: typeof Popcorn;
    popcorn: {
      atom: typeof atom;
      tuple: typeof tuple;
    };
  }
}

type InitOptions = PopcornOpts;
type BootResult = Result<null>;
type CallOptions = { timeoutMs?: number };
type EventWaiter = (event: PopcornEvent) => void;
type OtpFactory = (id?: string) => Promise<OtpHandle>;
type Otp = {
  id: string;
  events: PopcornEvent[];
  boot(options: InitOptions): Promise<BootResult>;
  send(target: string | Pid, payload?: unknown): Promise<BootResult>;
  genserver: {
    call(
      target: string | Pid,
      request?: unknown,
      options?: CallOptions,
    ): Promise<Result<unknown>>;
    cast(target: string | Pid, request?: unknown): Promise<BootResult>;
  };
  waitForEvent(name: string): Promise<PopcornEvent>;
  eventValue(name: string): unknown;
  deinit(): void;
};

export type Result<T> =
  { ok: true; data: T } | { ok: false; error: SerializedError };

export { assert, expect };

export function evalOpts(code: string): PopcornOpts {
  return {
    beam: {
      manifestUrl: "/assets/otp/manifest.json",
      extraArgs: ["-eval", trimLeft(code)],
    },
  };
}

export const test = base.extend<Fixtures>({
  page: async ({ page }, use) => {
    await page.goto("/");
    await page.waitForFunction(() => window.Popcorn !== undefined);
    await use(page);
  },
  createOtp: async ({ page }, use) => {
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
  public readonly genserver = {
    call: async (
      target: string | JSHandle<Pid>,
      request?: unknown,
      options?: CallOptions,
    ): Promise<Result<unknown>> =>
      await this.otp.evaluate(
        (otp, args) => otp.genserver.call(args.target, args.request, args.options),
        { target, request, options },
      ),
    cast: async (
      target: string | JSHandle<Pid>,
      request?: unknown,
    ): Promise<BootResult> =>
      await this.otp.evaluate(
        (otp, args) => otp.genserver.cast(args.target, args.request),
        { target, request },
      ),
  };
  private otpHandle: JSHandle<Otp> | null;

  private constructor(
    public readonly id: string,
    otp: JSHandle<Otp>,
  ) {
    this.otpHandle = otp;
  }

  public static async create(page: Page, id: string): Promise<OtpHandle> {
    const otp = await page.evaluateHandle(createOtp, id);
    return new OtpHandle(id, otp);
  }

  public async boot(options: InitOptions): Promise<BootResult> {
    const result = await this.otp.evaluate(
      (otp, initOptions) => otp.boot(initOptions),
      options,
    );
    await this.syncEvents();
    return result;
  }

  public async send(
    target: string | JSHandle<Pid>,
    payload?: unknown,
  ): Promise<BootResult> {
    const result = await this.otp.evaluate(
      (otp, args) => otp.send(args.target, args.payload),
      { target, payload },
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

  public async eventValueHandle<T>(name: string): Promise<JSHandle<T>> {
    await this.waitForEvent(name);
    return await this.otp.evaluateHandle(
      (otp, eventName) => otp.eventValue(eventName) as T,
      name,
    );
  }

  public async deinit(): Promise<void> {
    await this.otp.evaluate((otp) => otp.deinit());
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

  function check(condition: boolean, message: string): asserts condition {
    if (!condition) throw new Error(message);
  }

  function hasKey(
    value: unknown,
    key: string,
  ): value is Record<string, unknown> {
    return (
      typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, key)
    );
  }

  class Otp {
    public readonly id = id;
    public readonly events: PopcornEvent[] = [];
    public readonly genserver = {
      call: async (
        target: string | Pid,
        request?: unknown,
        options?: CallOptions,
      ): Promise<Result<unknown>> => {
        const result = await this.popcorn.genserver.call(
          target,
          request,
          options,
        );
        if (result.ok) return result;
        return { ok: false, error: result.error.serialize() };
      },
      cast: async (
        target: string | Pid,
        request?: unknown,
      ): Promise<BootResult> => {
        const result = await this.popcorn.genserver.cast(target, request);
        if (result.ok) return result;
        return { ok: false, error: result.error.serialize() };
      },
    };

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

    public async send(
      target: string | Pid,
      payload?: unknown,
    ): Promise<BootResult> {
      const result = await this.popcorn.send(target, payload);
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

    public eventValue(name: string): unknown {
      const event = this.findEvent(name);
      check(event !== null, `Missing event: ${name}`);
      check(hasKey(event, name), `Missing event value: ${name}`);
      return event[name];
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
