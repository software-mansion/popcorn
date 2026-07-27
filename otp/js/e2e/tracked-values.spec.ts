import type { Page } from "@playwright/test";
import { assert, evalOpts, expect, test } from "./helpers";

type PopcornHooks = {
  cleanups: number;
  cleanup: () => void;
  runJs: {
    isPaused: () => boolean;
    pause: () => Promise<void>;
    finish: () => void;
  };
};

declare global {
  var popcorn: PopcornHooks;
  var popcornCleanup: () => void;
}

test.describe("tracked values", () => {
  test("identity", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
          Tracked = wasm:run_js(
            <<"() => new TrackedValue({n: 1})">>,
            #{}
          ),
          Mutated = wasm:run_js(
            <<"({tracked}) => ++tracked.n">>,
            #{tracked => Tracked}
          ),
          Ref = wasm:run_js(
            <<"() => document.createElement('div')">>,
            #{},
            [{return, ref}]
          ),
          Tag = wasm:run_js(<<"({ref}) => ref.tagName">>, #{ref => Ref}),
          Wrapped = wasm:run_js(
            <<"() => new TrackedValue({n: 5})">>,
            #{},
            [{return, ref}]
          ),
          Nested = wasm:run_js(
            <<"(args) => args.list[0].n + args.wrap.value.n">>,
            #{list => [Wrapped], wrap => #{value => Wrapped}}
          ),
          ok = wasm:send(#{
            mutated => Mutated,
            tag => Tag,
            nested => Nested
          }).
        `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("mutated")).toEqual({
      mutated: 2,
      tag: "DIV",
      nested: 10,
    });
  });

  test("final send", async ({ otp, page }) => {
    await addHooks(page);
    const count = 8;
    const boot = await otp.boot(
      evalOpts(`
          spawn(fun() ->
            lists:foreach(fun(I) ->
              H = wasm:run_js(
                <<"({i}) => new TrackedValue(
                  {label: 'tracked-' + i},
                  () => globalThis.popcorn.cleanup()
                )">>,
                #{i => I}
              ),
              ok = wasm:send(#{final_ref => H}),
              erlang:garbage_collect(self())
            end, lists:seq(1, ${count})),
            ok = wasm:send(#{final_ref_done => true})
          end).
        `),
    );
    assert(boot.ok);

    await otp.waitForEvent("final_ref_done");
    const refs = valuesWithKey(otp.events, "final_ref");
    expect(valuesWithKey(refs, "label")).toEqual(
      Array.from({ length: count }, (_, index) => `tracked-${index + 1}`),
    );
    await expect.poll(() => cleanups(page)).toBe(count);
  });

  test("async argument", async ({ otp, page }) => {
    await addHooks(page);
    const boot = await otp.boot(
      evalOpts(`
          register(controller, self()),
          Runner = spawn(fun() ->
            receive
              {tracked, H} ->
                V = wasm:run_js(
                  <<"async ({h}) => {
                    await globalThis.popcorn.runJs.pause();
                    return {
                      value: h.value,
                      cleanups: globalThis.popcorn.cleanups
                    };
                  }">>,
                  #{h => H},
                  [{timeout, 10000}]
                ),
                ok = wasm:send(#{async_tracked => V})
            end
          end),
          spawn(fun() ->
            H = wasm:run_js(
              <<"() => new TrackedValue(
                {value: 'tracked argument'},
                () => globalThis.popcorn.cleanup()
              )">>,
              #{}
            ),
            Runner ! {tracked, H},
            ok = wasm:send(#{runner_ready => true})
          end),
          receive
            {wasm, #{<<"collect">> := true}} ->
              erlang:garbage_collect(Runner),
              ok = wasm:send(#{runner_collected => true})
          end.
        `),
    );
    assert(boot.ok);

    await otp.waitForEvent("runner_ready");
    await page.waitForFunction(() => globalThis.popcorn.runJs.isPaused());
    assert((await otp.send("controller", { collect: true })).ok);
    await otp.waitForEvent("runner_collected");
    await expect
      .poll(() => cleanups(page), { timeout: 500, intervals: [50] })
      .toBe(0);

    await page.evaluate(() => globalThis.popcorn.runJs.finish());
    expect(await otp.waitForEvent("async_tracked")).toEqual({
      async_tracked: { value: "tracked argument", cleanups: 0 },
    });
    await expect.poll(() => cleanups(page)).toBe(1);
  });

  test("instances", async ({ createOtp, page }) => {
    const [closed, live] = await Promise.all([createOtp(), createOtp()]);
    let cleanupCalls = 0;
    await page.exposeFunction("popcornCleanup", () => {
      cleanupCalls += 1;
    });
    await page.evaluate(() => {
      globalThis.popcorn = {
        cleanups: 0,
        cleanup: globalThis.popcornCleanup,
        runJs: {
          isPaused: () => false,
          pause: async () => {},
          finish: () => {},
        },
      };
    });

    const closedBoot = closed.boot(
      evalOpts(`
          spawn(fun() ->
            H = wasm:run_js(
              <<"() => new TrackedValue(
                {id: 'A'},
                () => globalThis.popcorn.cleanup()
              )">>,
              #{}
            ),
            ok = wasm:send(#{closed_ready => true}),
            receive stop -> H end
          end).
        `),
    );
    const liveBoot = live.boot(
      evalOpts(`
          true = register(controller, self()),
          H = wasm:run_js(
            <<"() => new TrackedValue({id: 'B'})">>,
            #{}
          ),
          ok = wasm:send(#{live_ready => true}),
          receive
            {wasm, _} -> ok = wasm:send(H)
          end.
        `),
    );
    assert((await closedBoot).ok);
    assert((await liveBoot).ok);
    await closed.waitForEvent("closed_ready");
    await live.waitForEvent("live_ready");

    await closed.dispose();
    await expect.poll(() => cleanupCalls).toBe(1);
    assert((await live.send("controller", {})).ok);
    expect(await live.waitForEvent("id")).toEqual({ id: "B" });
    expect(cleanupCalls).toBe(1);
  });
});

async function addHooks(page: Page): Promise<void> {
  await page.evaluate(() => {
    let paused = false;
    let finish!: () => void;
    const resume = new Promise<void>((resolve) => {
      finish = resolve;
    });
    globalThis.popcorn = {
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
        finish,
      },
    };
  });
}

async function cleanups(page: Page): Promise<number> {
  return await page.evaluate(() => globalThis.popcorn.cleanups);
}

function valuesWithKey<K extends PropertyKey>(
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
