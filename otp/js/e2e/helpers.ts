import assert from "node:assert/strict";
import {
  expect,
  test as base,
  type JSHandle,
  type Page,
} from "@playwright/test";
import type {
  Popcorn,
  PopcornOpts,
  SerializedError,
} from "@swmansion/popcorn-otp";

declare global {
  // eslint-disable-next-line @typescript-eslint/consistent-type-definitions
  interface Window {
    Popcorn: typeof Popcorn;
  }
}

type InitOptions = PopcornOpts;

export type Result<T> =
  | { ok: true; data: T }
  | { ok: false; error: SerializedError };

type BrowserInitResult = Result<Popcorn>;
type PopcornHandleResult = Result<JSHandle<Popcorn>>;
type PopcornInputContext = Record<string, unknown> & {
  popcorn?: never;
};
type PopcornEvaluateArg<TContext extends PopcornInputContext> = TContext & {
  popcorn: JSHandle<Popcorn>;
};

type PopcornContext<TContext extends PopcornInputContext> = TContext & {
  popcorn: Popcorn;
};

type PopcornRun<TContext extends PopcornInputContext, TResult> = (
  args: PopcornContext<TContext>,
) => Promise<TResult> | TResult;

export { assert, expect };

export const test = base.extend({
  page: async ({ page }, use) => {
    await page.goto("/");
    await page.waitForFunction(() => window.Popcorn !== undefined);
    await use(page);
  },
});

export async function withPopcorn<
  TContext extends PopcornInputContext,
  TResult,
>(
  page: Page,
  options: InitOptions,
  context: TContext,
  run: PopcornRun<TContext, TResult>,
): Promise<Result<TResult>> {
  const popcorn = await createPopcornHandle(page, options);
  if (!popcorn.ok) {
    return popcorn;
  }

  try {
    const data = await page.evaluate<TResult, PopcornEvaluateArg<TContext>>(
      run,
      { ...context, popcorn: popcorn.data },
    );
    return { ok: true, data };
  } finally {
    await disposePopcorn(page, popcorn.data);
  }
}

async function createPopcornHandle(
  page: Page,
  options: InitOptions,
): Promise<PopcornHandleResult> {
  const resultHandle = await page.evaluateHandle(
    async (initOptions: InitOptions): Promise<BrowserInitResult> => {
      const result = await window.Popcorn.init(initOptions);

      return result.ok
        ? { ok: true, data: result.data }
        : { ok: false, error: result.error.serialize() };
    },
    options,
  );

  const okHandle = await resultHandle.getProperty("ok");
  const ok = (await okHandle.jsonValue()) as boolean;
  await okHandle.dispose();

  if (!ok) {
    const errorHandle = await resultHandle.getProperty("error");
    const error = (await errorHandle.jsonValue()) as SerializedError;
    await errorHandle.dispose();
    await resultHandle.dispose();
    return { ok: false, error };
  }

  const popcorn = (await resultHandle.getProperty("data")) as JSHandle<Popcorn>;
  await resultHandle.dispose();
  return { ok: true, data: popcorn };
}

async function disposePopcorn(page: Page, popcorn: JSHandle<Popcorn>) {
  await page.evaluate((instance: Popcorn) => {
    instance.deinit();
  }, popcorn);
  await popcorn.dispose();
}
