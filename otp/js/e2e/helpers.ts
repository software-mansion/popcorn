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
    __recordOtpEvent: (event: PopcornEvent) => Promise<void>;
  }
}

type InitOptions = PopcornOpts;
type BootResult = Result<null>;
type EventWaiter = (event: PopcornEvent) => void;

export type Result<T> =
  | { ok: true; data: T }
  | { ok: false; error: SerializedError };

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

export class Otp {
  public readonly events = new Set<PopcornEvent>();

  private popcorn: JSHandle<Popcorn> | null = null;
  private readonly eventWaiters = new Map<string, Array<EventWaiter>>();

  public constructor(private readonly page: Page) {}

  public async installEventBridge(): Promise<void> {
    await this.page.exposeBinding("__recordOtpEvent", (_source, event) => {
      this.recordEvent(event as PopcornEvent);
    });
  }

  public async boot(options: InitOptions): Promise<BootResult> {
    if (this.popcorn !== null) {
      throw new Error("OTP is already booted");
    }

    this.popcorn = await this.page.evaluateHandle(
      (initOptions: InitOptions): Popcorn => new window.Popcorn(initOptions),
      options,
    );
    await this.popcorn.evaluate((popcorn) => {
      popcorn.onEvent((event) => {
        window.__recordOtpEvent(event);
      });
    });

    const result = await this.popcorn.evaluate(
      async (popcorn): Promise<BootResult> => {
        const boot = await popcorn.boot();
        if (boot.ok) return { ok: true, data: null };
        return { ok: false, error: boot.error.serialize() };
      },
    );

    if (!result.ok) {
      await this.dispose();
    }

    return result;
  }

  public async send(
    target: string | BeamTarget,
    payload?: unknown,
    opts?: PopcornSendOpts,
  ): Promise<BootResult> {
    const popcorn = this.requirePopcorn();

    return await popcorn.evaluate(
      async (instance, args) => {
        const result = await instance.send(
          args.target,
          args.payload,
          args.opts,
        );

        if (result.ok) return { ok: true, data: null };
        return { ok: false, error: result.error.serialize() };
      },
      { target, payload, opts },
    );
  }

  public async waitForEvent(name: string): Promise<PopcornEvent> {
    const event = this.findEvent(name);
    if (event !== null) {
      return event;
    }

    return await new Promise<PopcornEvent>((resolve) => {
      const waiters = this.eventWaiters.get(name) ?? [];
      waiters.push(resolve);
      this.eventWaiters.set(name, waiters);
    });
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
    const popcorn = this.popcorn;
    this.popcorn = null;
    if (popcorn !== null) {
      await this.page.evaluate((instance) => instance.deinit(), popcorn);
      await popcorn.dispose();
    }
  }

  private requirePopcorn(): JSHandle<Popcorn> {
    if (this.popcorn === null) {
      throw new Error("Popcorn has not been booted");
    }

    return this.popcorn;
  }

  private recordEvent(event: PopcornEvent): void {
    this.events.add(event);

    for (const [name, waiters] of this.eventWaiters) {
      if (
        typeof event === "object" &&
        event !== null &&
        Object.hasOwn(event, name)
      ) {
        this.eventWaiters.delete(name);
        for (const resolve of waiters) {
          resolve(event);
        }
      }
    }
  }

  private findEvent(name: string): PopcornEvent | null {
    for (const event of this.events) {
      if (
        typeof event === "object" &&
        event !== null &&
        Object.hasOwn(event, name)
      ) {
        return event;
      }
    }

    return null;
  }
}

type Fixtures = {
  otp: Otp;
};

export { assert, expect };

export const test = base.extend<Fixtures>({
  page: async ({ page }, use) => {
    await page.goto("/");
    await page.waitForFunction(() => window.Popcorn !== undefined);
    await use(page);
  },
  otp: async ({ page }, use) => {
    const otp = new Otp(page);
    await otp.installEventBridge();
    await use(otp);
    await otp.dispose();
  },
});
