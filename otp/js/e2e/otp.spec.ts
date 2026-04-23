import { test, expect, type Page } from "@playwright/test";

type SerializedError = {
  name: string;
  message: string;
};

type BootEvent = { type: string; data: unknown };

type BootResult = {
  ok: boolean;
  events: Array<BootEvent>;
  module: { args: string[] } | null;
  error: SerializedError | null;
};

const WORKER_URL = "/otp-worker.ts";

function getAbortEvent(events: BootEvent[]) {
  return events.find((event) => event.type === "otp:abort") ?? null;
}

test("boots with the packaged OTP assets", async ({ page }) => {
  await page.goto("/");
  const result = await bootInWorker(page, "/otp-assets");

  expect(result.ok).toBe(true);
  expect(result.module).not.toBeNull();
  assert(result.module !== null);
  expect(result.module.args).toContain("-boot");
  expect(result.module.args).toContain("vm");
  expect(getAbortEvent(result.events)).toBeNull();
});

test("reports a missing boot script when assets are absent", async ({
  page,
}) => {
  await page.goto("/");
  const result = await bootInWorker(page, "/otp-assets/missing");

  expect(result.error).not.toBeNull();
  assert(result.error !== null);
  expect(result.error.message).toContain(
    "Missing boot script: '/otp-assets/missing/bin/vm.boot'",
  );
  expect(result.events).toContainEqual({
    type: "otp:abort",
    data: "Missing boot script: '/otp-assets/missing/bin/vm.boot'",
  });
});

async function bootInWorker(
  page: Page,
  assetsUrl: string,
): Promise<BootResult> {
  return await page.evaluate(
    async ({ workerUrl, assetsUrl }): Promise<BootResult> => {
      const worker = new Worker(workerUrl, { type: "module" });
      const events: Array<BootEvent> = [];

      return await new Promise<BootResult>((resolve) => {
        worker.addEventListener(
          "message",
          ({ data: { type, data } }: MessageEvent<BootEvent>) => {
            if (type === "boot:ok") {
              worker.terminate();
              const payload = {
                ok: true,
                events,
                module: data as BootResult["module"],
                error: null,
              };
              resolve(payload);
              return;
            } else if (type === "boot:error") {
              worker.terminate();
              const payload = {
                ok: false,
                events,
                module: null,
                error: data as SerializedError,
              };
              resolve(payload);
              return;
            }

            events.push({ type, data });
          },
        );

        worker.addEventListener("error", (event) => {
          worker.terminate();
          const payload = {
            ok: false,
            events,
            module: null,
            error: {
              name: "WorkerError",
              message: event.message || "Worker failed to load",
            },
          };
          resolve(payload);
        });

        worker.postMessage({ type: "boot", assetsUrl });
      });
    },
    { workerUrl: WORKER_URL, assetsUrl },
  );
}

function assert(ok: boolean): asserts ok {
  if (!ok) throw Error("assert");
}
