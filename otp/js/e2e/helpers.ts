import assert from "node:assert/strict";
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
  BeamTarget,
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
type OtpFactory = () => Promise<OtpHandle>;
type Otp = {
  events: PopcornEvent[];
  boot(options: InitOptions): Promise<BootResult>;
  send(
    target: string | BeamTarget,
    payload?: unknown,
    opts?: PopcornSendOpts,
  ): Promise<BootResult>;
  waitForEvent(name: string): Promise<PopcornEvent>;
  deinit(): void;
};

export type Result<T> =
  { ok: true; data: T } | { ok: false; error: SerializedError };

export { assert, expect };

export const test = base.extend<Fixtures>({
  page: async ({ page }, use) => {
    await page.goto("/");
    await page.waitForFunction(() => window.Popcorn !== undefined);
    await use(page);
  },
  createOtp: async ({ page }, use) => {
    const handles = new Set<OtpHandle>();
    const createOtp = async () => {
      const otp = await OtpHandle.create(page);
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
    otp: JSHandle<Otp>,
  ) {
    this.otpHandle = otp;
  }

  public static async create(page: Page): Promise<OtpHandle> {
    const otp = await page.evaluateHandle(createOtp);
    return new OtpHandle(page, otp);
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
    target: string | BeamTarget,
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

  public async waitForStderr(text: string): Promise<ConsoleMessage> {
    return await this.page.waitForEvent("console", (message) => {
      return (
        message.type() === "error" &&
        message.text().includes("[Popcorn]") &&
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

function createOtp(): Otp {
  function hasKey(event: PopcornEvent, key: string) {
    return (
      typeof event === "object" && event !== null && Object.hasOwn(event, key)
    );
  }

  function check(condition: boolean, message: string): asserts condition {
    if (!condition) throw new Error(message);
  }

  class Otp {
    public readonly events: PopcornEvent[] = [];

    private popcornHandle: Popcorn | null = null;
    private readonly eventWaiters = new Map<string, Array<EventWaiter>>();

    public async boot(options: InitOptions): Promise<BootResult> {
      check(this.popcornHandle === null, "OTP is already booted");

      this.popcornHandle = new window.Popcorn(options);
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
      target: string | BeamTarget,
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
