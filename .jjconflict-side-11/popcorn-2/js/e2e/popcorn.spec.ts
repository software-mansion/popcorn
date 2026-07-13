import { test, expect, Page, JSHandle } from "@playwright/test";
import { Popcorn, CallOptions } from "@swmansion/popcorn";

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    Popcorn: typeof Popcorn;
    captureStdout?: (msg: string) => void;
    serializeCallResult: typeof serializeCallResult;
    receivedEvents: unknown[];
    unsubscribeOnMessage: () => void;
  }
}

function serializeCallResult<T extends { ok: boolean; error?: unknown }>(
  result: T,
) {
  if (!result.ok && result.error) {
    const e = result.error as Error & { code?: string };
    return {
      ...result,
      error: { name: e.name, message: e.message, code: e.code },
    };
  }
  return result;
}

class PopcornHandle {
  constructor(private handle: JSHandle<Popcorn>) {}

  async call(payload: unknown, opts?: CallOptions) {
    return this.handle.evaluate(
      async (p, args) =>
        serializeCallResult(await p.call(args.payload, args.opts)),
      { payload, opts },
    );
  }

  async deinit() {
    return this.handle.evaluate((p) => p.deinit());
  }

  evaluate<T>(fn: (p: Popcorn) => T | Promise<T>): Promise<T>;
  evaluate<T, A>(
    fn: (p: Popcorn, arg: A) => T | Promise<T>,
    arg: A,
  ): Promise<T>;
  evaluate(
    fn: (p: Popcorn, ...args: unknown[]) => unknown,
    ...args: unknown[]
  ) {
    return this.handle.evaluate(fn, ...args);
  }
}

test.beforeEach(async ({ page }) => {
  await page.addInitScript(
    `window.serializeCallResult = ${serializeCallResult.toString()}`,
  );
  await page.goto("/");
  await page.waitForFunction(() => window.Popcorn !== undefined);
});

test("init", async ({ page }) => {
  const popcorn = await initPopcorn(page);

  const iframe = page.locator("#popcorn-container iframe");
  await expect(iframe).toBeAttached();

  const isPopcorn = await popcorn.evaluate(
    (p) => p.constructor.name === "Popcorn",
  );
  expect(isPopcorn).toBe(true);
});

test("call echo", async ({ page }) => {
  const popcorn = await initPopcorn(page);

  const echoPayload = {
    action: "echo",
    data: { message: "hello world", number: 42 },
  };
  const echoResponse = {
    echoed: { message: "hello world", number: 42 },
  };

  const result = await popcorn.call(echoPayload);

  expect(result.ok).toBe(true);
  assert(result.ok);
  expect(result.data).toEqual(echoResponse);
  expect(result.durationMs).toBeGreaterThanOrEqual(0);
});

test("call complex payload", async ({ page }) => {
  const popcorn = await initPopcorn(page);

  const complexData = {
    string: "hello",
    number: 42,
    float: 3.14,
    boolean: true,
    null: null,
    array: [1, 2, 3, "four", null, { nested: true }],
    object: {
      nested: {
        deeply: {
          value: "found",
        },
      },
      empty: {},
      list: [],
    },
  };

  const payload = { action: "echo", data: complexData };
  const result = await popcorn.call(payload);

  expect(result.ok).toBe(true);
  assert(result.ok);
  expect(result.data).toEqual({ echoed: complexData });
});

test("call delay", async ({ page }) => {
  const popcorn = await initPopcorn(page);

  const payload = {
    action: "delay",
    ms: 500,
    data: "delayed data",
  };
  const response = { delayed: "delayed data", ms: 500 };
  const result = await popcorn.call(payload);

  expect(result.ok).toBe(true);
  assert(result.ok);
  expect(result.data).toEqual(response);
  expect(result.durationMs).toBeGreaterThanOrEqual(500);
});

test("call timeout", async ({ page }) => {
  const popcorn = await initPopcorn(page);

  const payload = { action: "delay", ms: 5_000, data: "will timeout" };
  const result = await popcorn.call(payload, { timeoutMs: 500 });

  expect(result.ok).toBe(false);
  assert(!result.ok);
  expect(result.error.name).toBe("PopcornError");
  expect(result.error.code).toBe("timeout");
  expect(result.error.message).toContain("timeout");
});

test("stdout", async ({ page }) => {
  const stdoutMessages: string[] = [];
  await page.exposeFunction("captureStdout", (msg: string) => {
    stdoutMessages.push(msg);
  });

  const handle = await page.evaluateHandle(async () => {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const container = document.getElementById("popcorn-container")!;
    return window.Popcorn.init({
      container,
      onStdout: (msg: string) => window.captureStdout?.(msg),
    });
  });
  const popcorn = new PopcornHandle(handle);

  const payload = {
    action: "stdout",
    message: "Hello from E2E test!",
  };
  await popcorn.call(payload);
  expect(stdoutMessages).toContain("Hello from E2E test!");
});

test("deinit", async ({ page }) => {
  const popcorn = await initPopcorn(page);

  const payload = {
    action: "delay",
    ms: 5_000,
    data: "will be cancelled",
  };
  const { iframe, callResult } = await popcorn.evaluate(async (p, pl) => {
    const callPromise = p.call(pl);
    p.deinit();

    const iframe = document.querySelector("#popcorn-container iframe");
    const callResult = serializeCallResult(await callPromise);

    return { iframe, callResult };
  }, payload);

  expect(iframe).toBe(null);
  expect(callResult.ok).toBe(false);
  assert(!callResult.ok);
  expect(callResult.error.name).toBe("PopcornError");
  expect(callResult.error.code).toBe("deinitialized");
  expect(callResult.durationMs).toBeGreaterThanOrEqual(0);
});

test("send_event", async ({ page }) => {
  const { sendEvent, waitForEvents } = await initPopcornWithEventListener(page);

  await sendEvent("test_event", { message: "hello from elixir" });

  const events = await waitForEvents(1);
  expect(events).toEqual([
    { name: "test_event", payload: { message: "hello from elixir" } },
  ]);
});

test("onMessage unsubscribe", async ({ page }) => {
  const { sendEvent, waitForEvents } = await initPopcornWithEventListener(page);

  await sendEvent("event_1", null);
  await waitForEvents(1);

  await page.evaluate(() => window.unsubscribeOnMessage());

  await sendEvent("event_2", null);
  await page.waitForTimeout(500);

  const events = await page.evaluate(() => window.receivedEvents);
  expect(events).toEqual([{ name: "event_1", payload: null }]);
});

async function initPopcornWithEventListener(page: Page) {
  const handle = await page.evaluateHandle(async () => {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const container = document.getElementById("popcorn-container")!;
    const p = await window.Popcorn.init({ container });
    window.receivedEvents = [];
    window.unsubscribeOnMessage = p.onMessage(
      (name: string, payload: unknown) => {
        window.receivedEvents.push({ name, payload });
      },
    );
    return p;
  });
  const popcorn = new PopcornHandle(handle);

  async function sendEvent(name: string, payload: unknown) {
    await popcorn.call({ action: "send_event", name, payload });
  }

  async function waitForEvents(count: number) {
    const handle = await page.waitForFunction(
      (n) => {
        if (window.receivedEvents.length < n) return null;
        return window.receivedEvents.slice(0, n);
      },
      count,
      { timeout: 5_000 },
    );
    return handle.jsonValue();
  }

  return { popcorn, sendEvent, waitForEvents };
}

async function initPopcorn(page: Page): Promise<PopcornHandle> {
  const handle = await page.evaluateHandle(async () => {
    // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
    const container = document.getElementById("popcorn-container")!;
    return window.Popcorn.init({ container });
  });
  return new PopcornHandle(handle);
}

function assert(p: boolean): asserts p {
  if (!p) throw new Error("assert");
}
