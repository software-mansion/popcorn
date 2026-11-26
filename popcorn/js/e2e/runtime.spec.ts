import { schedulers, type PopcornOpts } from "@swmansion/popcorn";
import { assert, evalOpts, expect, test } from "./helpers";
import { PopcornError, type SerializedError } from "../src/errors";
import { applyDefaults } from "../src/beam";
import { Assets } from "../src/assets";
import type { Page } from "@playwright/test";
import { mock } from "node:test";

const FETCH_URL = "/assets/otp/manifest.json";
type MemoryProbe = {
  descriptor: WebAssembly.MemoryDescriptor;
  memory: WebAssembly.Memory;
};

test.describe("errors", () => {
  test("round trip", () => {
    const errors: SerializedError[] = [
      { t: "timeout", data: { operation: "send", timeoutMs: 1 } },
      { t: "timeout", data: { operation: "call", timeoutMs: 1 } },
      { t: "genserver:noproc", data: { target: "missing" } },
      { t: "genserver:exit", data: { reason: "shutdown" } },
      { t: "genserver:unserializable", data: {} },
      { t: "asset:load", data: { kind: "tarball", url: "/app.tar" } },
    ];

    for (const serialized of errors) {
      const original = new PopcornError(serialized);
      const restored = PopcornError.deserialize(
        structuredClone(original.serialize()),
      );
      expect(restored.serialize()).toEqual(serialized);
    }

    expect(() => PopcornError.deserialize({ t: "timeout", data: {} })).toThrow(
      Error,
    );
  });
});

test.describe("boot", () => {
  test("cached assets", async ({ page, createOtp }) => {
    const cache = await installAssetCache(page);
    const expectedHashes = await assetHashes(page);

    const first = await createOtp();
    expect(await first.tryBoot({})).toEqual({ ok: true, data: null });
    await expect.poll(cache.hashes).toEqual(expectedHashes);
    await first.dispose();

    const downloads: string[] = [];
    await page.route(/\.(wasm|tar)(\?|$)/, (route) => {
      downloads.push(route.request().url());
      return route.abort();
    });

    const second = await createOtp();
    expect(await second.tryBoot({})).toEqual({ ok: true, data: null });
    expect(downloads).toEqual([]);
    expect(await cache.hashes()).toEqual(expectedHashes);
  });

  // The VM's binary downloads alongside the bundles, not after them: here the
  // bundles finish downloading only once it has started
  test("parallel downloads", async ({ page, otp }) => {
    let wasmRequested!: () => void;
    const wasm = new Promise<void>((resolve) => (wasmRequested = resolve));
    await page.route(/\.wasm(\?|$)/, (route) => {
      wasmRequested();
      return route.continue();
    });
    await page.route(/\.tar(\?|$)/, async (route) => {
      await wasm;
      return route.continue();
    });
    expect(await otp.tryBoot({})).toEqual({ ok: true, data: null });
  });

  test("page extension", async ({ page, otp, createOtp }) => {
    const cache = await installAssetCache(page, "page");
    const hashes = await assetHashes(page);
    const downloads: string[] = [];
    page.on("response", (response) => {
      if (/\.(wasm|tar)$/.test(new URL(response.url()).pathname))
        downloads.push(response.url());
    });

    expect(await otp.tryBoot({})).toEqual({ ok: true, data: null });
    await expect.poll(cache.hashes).toEqual(hashes);
    expect(downloads).toHaveLength(hashes.length);
    await otp.deinit();
    downloads.length = 0;

    const assetRequests = /\.(wasm|tar)(\?|$)/;
    await page.route(assetRequests, (route) => route.abort());
    expect(await otp.tryBoot({})).toEqual({ ok: true, data: null });
    expect(downloads).toEqual([]);
    await otp.deinit();
    await page.unroute(assetRequests);

    const disabled = await createOtp();
    expect(
      await disabled.tryBoot({ beam: { crossOriginStorage: false } }),
    ).toEqual({
      ok: true,
      data: null,
    });
    expect(downloads).toHaveLength(hashes.length);
  });

  test("memory limits", async ({ page, createOtp }) => {
    await page.route("**/worker.mjs*", async (route) => {
      const response = await route.fetch();
      const source = await response.text();
      await route.fulfill({
        response,
        body: `
          WebAssembly.Memory = new Proxy(WebAssembly.Memory, {
            construct(target, args) {
              const memory = Reflect.construct(target, args);
              self.memoryProbe = { descriptor: args[0], memory };
              return memory;
            }
          });
          ${source}
        `,
      });
    });

    const cases: {
      userAgent: string;
      touch: number;
      maximum: number;
      maxMemory?: NonNullable<PopcornOpts["beam"]>["maxMemory"];
      grow?: boolean;
    }[] = [
      {
        userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X)",
        touch: 0,
        maximum: 32768,
        maxMemory: { mobile: "128M" },
      },
      {
        userAgent: "Mozilla/5.0 (iPhone; CPU iPhone OS) Mobile",
        touch: 5,
        maximum: 4096,
        maxMemory: { mobile: "256M", desktop: "2G" },
      },
      { userAgent: "Mozilla/5.0 (Linux; Android 16)", touch: 5, maximum: 4096 },
      {
        userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X)",
        touch: 5,
        maximum: 4096,
        maxMemory: { desktop: "1G" },
      },
      {
        userAgent: "Mozilla/5.0 (iPhone; CPU iPhone OS) Mobile",
        touch: 5,
        maximum: 2048,
        maxMemory: { mobile: "128M" },
        grow: true,
      },
      {
        userAgent: "Mozilla/5.0 (Macintosh; Intel Mac OS X)",
        touch: 0,
        maximum: 16384,
        maxMemory: { desktop: "1G" },
      },
    ];

    for (const device of cases) {
      await page.evaluate(({ userAgent, touch }) => {
        Object.defineProperty(navigator, "userAgent", {
          configurable: true,
          value: userAgent,
        });
        Object.defineProperty(navigator, "maxTouchPoints", {
          configurable: true,
          value: touch,
        });
      }, device);

      const otp = await createOtp();
      const workerStarted = page.waitForEvent("worker", (worker) =>
        worker.url().includes("worker.mjs"),
      );
      let extraArgs: string[] = [];
      if (device.grow) {
        extraArgs = [
          "-eval",
          `
          true = register(memory_probe, self()),
          Binary = binary:copy(<<0>>, 80 * 1024 * 1024),
          wasm:send(#{memory_growth => byte_size(Binary)}),
          receive {wasm, _} -> wasm:send(#{memory_kept => byte_size(Binary)}) end.
        `,
        ];
      }
      const boot = await otp.tryBoot({
        beam: { maxMemory: device.maxMemory, extraArgs },
      });
      assert(boot.ok);
      const worker = await workerStarted;
      const descriptor = await worker.evaluate(() => {
        const probe = (self as unknown as { memoryProbe: MemoryProbe })
          .memoryProbe;
        return probe.descriptor;
      });
      expect(descriptor).toEqual({
        initial: 1024,
        maximum: device.maximum,
        shared: true,
      });

      if (device.grow) {
        expect(await otp.waitForEvent("memory_growth")).toEqual({
          memory_growth: 80 * 1024 * 1024,
        });
        const growth = await worker.evaluate(() => {
          const probe = (self as unknown as { memoryProbe: MemoryProbe })
            .memoryProbe;
          const memory = probe.memory;
          const before = memory.buffer.byteLength;
          memory.grow(2048 - memory.buffer.byteLength / 65536);
          let capped = false;
          try {
            memory.grow(1);
          } catch (error) {
            capped = error instanceof RangeError;
          }
          return { before, bytes: memory.buffer.byteLength, capped };
        });
        expect(growth.before).toBeGreaterThan(64 * 1024 * 1024);
        expect(growth.bytes).toBe(device.maximum * 65536);
        expect(growth.capped).toBe(true);
        assert((await otp.trySend("memory_probe", true)).ok);
        expect(await otp.waitForEvent("memory_kept")).toEqual({
          memory_kept: 80 * 1024 * 1024,
        });
      }
      await otp.deinit();
    }

    const invalid = await page.evaluate(() =>
      ["256K", "3G", "64.01M"].map((size) => {
        try {
          new window.Popcorn({
            beam: { maxMemory: { mobile: size as `${number}M` } },
          });
          return null;
        } catch (error) {
          if (!(error instanceof Error)) throw error;
          return { name: error.name, message: error.message };
        }
      }),
    );
    expect(invalid).toEqual([
      {
        name: "Error",
        message: 'maxMemory must use M or G, such as "256M" or "2G"',
      },
      ...Array(2).fill({
        name: "Error",
        message:
          "maxMemory must be a multiple of 64 KiB between 64 MiB and 2 GiB",
      }),
    ]);
  });

  test("apps and eval", async ({ page, otp }) => {
    const archives: { url: string; encoding: string | undefined }[] = [];
    page.on("response", (response) => {
      if (new URL(response.url()).pathname.endsWith(".tar")) {
        archives.push({
          url: response.url(),
          encoding: response.headers()["content-encoding"],
        });
      }
    });
    const boot = await otp.tryBoot(
      evalOpts(`
        {ok, _} = application:ensure_all_started(elixir),
        {ok, _} = application:ensure_all_started(logger),
        3 = element(1, 'Elixir.Code':eval_string(<<"1 + 2">>)),
        _ = 'Elixir.Logger':level(),
        ok = wasm:send(#{
          runtime_ready => true,
          otp_release => list_to_binary(erlang:system_info(otp_release)),
          elixir_version => 'Elixir.System':version()
        }).
      `),
    );
    assert(boot.ok);

    expect(archives).toHaveLength(3);
    const groups = archives.map(({ url }) => {
      const filename = new URL(url).pathname.split("/").pop();
      assert(filename !== undefined);
      return filename.split(".")[0];
    });
    expect(groups.sort()).toEqual(["app", "deps", "stdlib"]);
    for (const archive of archives) {
      expect(archive.url).toMatch(/\.[a-f0-9]{64}\.tar$/);
      expect(["br", "gzip"]).toContain(archive.encoding);
    }

    expect(await otp.waitForEvent("runtime_ready")).toEqual({
      runtime_ready: true,
      otp_release: "29",
      elixir_version: "1.20.4",
    });
  });

  test("PAX module path", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
        42 = test_entrypoint_module_with_a_name_long_enough_for_a_posix_pax_extended_tar_header_and_cannot_fit_in_ustar_name:answer(),
        ok = wasm:send(#{pax_module => loaded}).
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("pax_module")).toEqual({
      pax_module: "loaded",
    });
  });

  test("startup bridge", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const events: unknown[] = [];
      let booted = false;
      const boot = await window.Popcorn.init({
        beam: {
          env: { POPCORN_STARTUP_EVENT: "bridge" },
        },
        onEvent: (event) => events.push({ event, beforeBoot: !booted }),
      });
      booted = true;
      boot.deinit();
      return {
        boot: true,
        events,
      };
    });

    expect(result).toEqual({
      boot: true,
      events: expect.arrayContaining([
        { event: { configured_answer: 42 }, beforeBoot: true },
        { event: { startup_send: true }, beforeBoot: true },
        { event: { startup_action: true }, beforeBoot: true },
        { event: { startup_run_js: 42 }, beforeBoot: true },
      ]),
    });
  });

  test("readiness", async ({ otp }) => {
    const boot = await otp.tryBoot({
      beam: { env: { POPCORN_STARTUP_EVENT: "await_ready" } },
    });
    assert(boot.ok);

    expect(await otp.waitForEvent("async_ready")).toEqual({
      async_ready: true,
    });
    assert((await otp.trySend("await_ready_task", {})).ok);
    expect(await otp.waitForEvent("after_boot")).toEqual({
      after_boot: true,
    });
  });

  test("readiness timeout", async ({ otp }) => {
    const boot = await otp.tryBoot({
      beam: { env: { POPCORN_STARTUP_EVENT: "await_ready_timeout" } },
    });
    assert(boot.ok);

    expect(await otp.waitForEvent("await_ready_timeout")).toEqual({
      await_ready_timeout: true,
    });
  });

  test("no options", async ({ page }) => {
    await page.route("/assets/otp/manifest.json", async (route) => {
      const response = await route.fetch();
      const manifest = (await response.json()) as Record<string, unknown>;
      await route.fulfill({
        response,
        json: { ...manifest, entrypoint: null },
      });
    });

    const result = await page.evaluate(async () => {
      const popcorn = new window.Popcorn();
      await popcorn.boot();
      popcorn.deinit();

      const init = await window.Popcorn.init();
      init.deinit();
      return true;
    });

    expect(result).toBe(true);
  });

  test("no entrypoint", async ({ otp }) => {
    // The fixture, cooked with `app: nil` by the global setup
    const boot = await otp.tryBoot({
      beam: {
        otpAssetsRoot: "/no-entrypoint/otp/",
        extraArgs: ["-run", "test_entrypoint_app", "report_no_entrypoint"],
      },
    });
    assert(boot.ok);

    expect(await otp.waitForEvent("no_entrypoint")).toEqual({
      no_entrypoint: { started: false, configured_answer: 42 },
    });
  });

  test("entrypoint failure", async ({ createOtp }) => {
    const otp = await createOtp();
    const boot = await otp.tryBoot({
      beam: {
        env: { POPCORN_STARTUP_EVENT: "fail" },
      },
    });

    expect(boot).toEqual({
      ok: false,
      error: { t: "vm:exited", data: { reason: "exit", data: 1 } },
    });
  });

  test("kernel failure", async ({ otp }) => {
    test.setTimeout(10_000);
    const boot = await otp.tryBoot({
      beam: { extraArgs: ["-user", "missing_user"] },
      timeoutsMs: { boot: 30_000, appStartup: 30_000 },
    });

    const diagnostic = expect.stringMatching(
      /Kernel pid terminated.*missing_user/s,
    );
    expect(boot).toEqual({
      ok: false,
      error: { t: "vm:exited", data: { reason: "error", data: diagnostic } },
    });
    expect(otp.errors).toEqual([{ kind: "error", data: diagnostic }]);
  });

  // Emulator flags, the scheduler counts they give, all of them online, and
  // the VM's threads. Popcorn's default is one scheduler of each type, and
  // emulator flags override only the counts they set. The BEAM takes flag
  // values apart or attached, and runs no more dirty CPU schedulers than
  // schedulers. Besides its schedulers, async threads and poll threads, the VM
  // starts 3 threads. Each thread is a worker, next to Popcorn's own, and the
  // thread pool fits them. A pool that's too small would end up with as many
  // workers, the missing ones started later, so the test checks the pool size
  // on its own.
  type Threading = { flags?: string[]; counts: number[]; threads: number };
  const threading: Record<string, Threading> = {
    "no flags": { counts: [1, 1, 1], threads: 8 },
    "other flags": { flags: ["-sbwt", "none"], counts: [1, 1, 1], threads: 8 },
    "one count": { flags: ["-S", "2"], counts: [2, 1, 1], threads: 9 },
    "values apart": {
      flags: [
        ...schedulers({ base: 2, dirtyCpu: 2, dirtyIo: 2 }),
        ...["-A", "4", "-IOt", "2"],
      ],
      counts: [2, 2, 2],
      threads: 15,
    },
    "values attached": {
      flags: ["-S2", "-SDcpu2", "-SDio2", "-A4", "-IOt2"],
      counts: [2, 2, 2],
      threads: 15,
    },
    "dirty CPU capped": {
      flags: ["-S2", "-SDcpu4", "-SDio2", "-A4", "-IOt2"],
      counts: [2, 2, 2],
      threads: 15,
    },
  };
  for (const [form, { flags, counts, threads }] of Object.entries(threading)) {
    test(`threads, ${form}`, async ({ otp, page }) => {
      const { threadPoolSize } = applyDefaults(flags ?? []);
      expect(threadPoolSize).toBe(threads);

      const boot = await otp.tryBoot({
        beam: {
          emulatorArgs: flags,
          extraArgs: [
            "-eval",
            `
              Info = fun erlang:system_info/1,
              ok = wasm:send(#{
                schedulers => Info(schedulers),
                schedulers_online => Info(schedulers_online),
                dirty_cpu_schedulers => Info(dirty_cpu_schedulers),
                dirty_cpu_schedulers_online => Info(dirty_cpu_schedulers_online),
                dirty_io_schedulers => Info(dirty_io_schedulers)
              }),
              receive stop -> ok end.
            `,
          ],
        },
      });
      assert(boot.ok);

      const [base, dirtyCpu, dirtyIo] = counts;
      expect(await otp.waitForEvent("schedulers")).toEqual({
        schedulers: base,
        schedulers_online: base,
        dirty_cpu_schedulers: dirtyCpu,
        dirty_cpu_schedulers_online: dirtyCpu,
        dirty_io_schedulers: dirtyIo,
      });
      await expect.poll(() => page.workers().length).toBe(1 + threads);
    });
  }

  test("errors", async ({ createOtp, page }) => {
    const timedOut = await createOtp();
    expect(await timedOut.tryBoot({ timeoutsMs: { boot: 0 } })).toEqual({
      ok: false,
      error: { t: "timeout", data: { operation: "init", timeoutMs: 0 } },
    });

    const missing = await createOtp();
    const missingBoot = await missing.tryBoot({
      beam: { otpAssetsRoot: "/missing/otp/" },
    });
    expect(missingBoot).toEqual({
      ok: false,
      error: {
        t: "asset:load",
        data: { kind: "manifest", url: "/missing/otp/manifest.json" },
      },
    });
    expect(missing.errors).toEqual([]);

    await page.route("**/rejected/manifest.json", (route) => route.abort());
    const rejected = await createOtp();
    const rejectedBoot = await rejected.tryBoot({
      beam: { otpAssetsRoot: "/rejected/" },
    });
    expect(rejectedBoot).toEqual({
      ok: false,
      error: {
        t: "asset:load",
        data: { kind: "manifest", url: "/rejected/manifest.json" },
      },
    });
    expect(rejected.errors).toEqual([]);

    for (const kind of ["manifest-entry", "tarball"]) {
      await page.route("**/asset-errors/**", (route) => {
        if (route.request().url().endsWith("manifest.json")) {
          return route.fulfill({
            json: {
              apps:
                kind === "manifest-entry"
                  ? {}
                  : {
                      kernel: { bundle: "core" },
                      stdlib: { bundle: "core" },
                      compiler: { bundle: "core" },
                    },
              bundles: { core: { tar: "kernel.tar" } },
              vm: { script: "vm/beam.emu.mjs", wasm: "vm/beam.wasm" },
            },
          });
        }
        return route.fulfill({ status: 404 });
      });
      const asset = await createOtp();
      const result = await asset.tryBoot({
        beam: { otpAssetsRoot: "/asset-errors/" },
      });
      const data =
        kind === "manifest-entry"
          ? {
              kind,
              url: "/asset-errors/manifest.json",
              name: "kernel",
              available: [],
            }
          : { kind, url: "/asset-errors/kernel.tar" };
      expect(result).toEqual({ ok: false, error: { t: "asset:load", data } });
      await page.unroute("**/asset-errors/**");
    }
  });

  test("native runtime error", async ({ page }) => {
    test.setTimeout(5_000);
    const result = await page.evaluate(async () => {
      const errors: unknown[] = [];
      const popcorn = await window.Popcorn.init({
        workerUrl: "/fault-worker.mjs?native=runtime",
        timeoutsMs: { send: 30_000 },
        onError: (error) => errors.push(error),
      });
      try {
        const pending = await Promise.all([
          popcorn.trySend("receiver", {}),
          popcorn.tryCall("receiver", {}, { timeoutMs: 30_000 }),
        ]);
        return {
          errors,
          pending: pending.map((result) => {
            if (result.ok) throw new Error("Expected worker failure");
            return result.error.serialize();
          }),
        };
      } finally {
        popcorn.deinit();
      }
    });

    const diagnostic = expect.stringContaining("native runtime failure");
    const failure = {
      t: "vm:exited",
      data: { reason: "error", data: diagnostic },
    };
    expect(result.pending).toEqual([failure, failure]);
    expect(result.errors).toEqual([{ kind: "error", data: diagnostic }]);
  });

  test("worker dispatcher rejection", async ({ page }) => {
    const started = page.waitForEvent("worker");
    const instance = await page.evaluateHandle(() => new window.Popcorn());
    const url = (await started).url();
    await instance.evaluate((popcorn) => popcorn.deinit());
    await instance.dispose();
    const result = await page.evaluate(async (url) => {
      const worker = new Worker(url, { type: "module" });
      try {
        return await new Promise((resolve) => {
          worker.onmessage = (event) => resolve(event.data);
          worker.postMessage({
            type: "popcorn:stdin",
            payload: { chunk: new Uint8Array([1]) },
          });
        });
      } finally {
        worker.terminate();
      }
    }, url);
    expect(result).toEqual({
      type: "otp:error",
      payload: { kind: "error", data: "Check failed" },
    });
  });

  test("worker boot errors", async ({ createOtp }) => {
    test.setTimeout(5_000);
    for (const [workerUrl, diagnostic] of [
      ["/fault-worker.mjs", { kind: "abort", data: "boom" }],
      [
        "/fault-worker.mjs?native=boot",
        { kind: "error", data: expect.stringContaining("native boot failure") },
      ],
    ] as const) {
      const otp = await createOtp();
      const boot = await otp.tryBoot({
        workerUrl,
        timeoutsMs: { boot: 30_000 },
      });
      expect(boot).toEqual({
        ok: false,
        error: {
          t: "vm:exited",
          data: { reason: diagnostic.kind, data: diagnostic.data },
        },
      });
      expect(otp.errors).toEqual([diagnostic]);
    }
  });
});

test.describe("runtime", () => {
  test("shutdown", async ({ createOtp }) => {
    for (const [expression, error] of [
      [
        'erlang:halt("fatal diagnostic")',
        { kind: "error", data: "fatal diagnostic\n" },
      ],
      ["erlang:halt(abort)", { kind: "error", data: "Erlang VM terminated" }],
      ["init:stop()", { kind: "exit", data: 0 }],
      ["erlang:halt(7)", { kind: "exit", data: 7 }],
    ] as const) {
      const otp = await createOtp();
      const boot = await otp.tryBoot(
        evalOpts(`
        true = register(shutdown_test, self()),
        wasm:send(#{shutdown_ready => true}),
        receive {wasm, _} -> ${expression} end.
      `),
      );
      assert(boot.ok);
      await otp.waitForEvent("shutdown_ready");

      const sent = await otp.trySend("shutdown_test", true);
      assert(sent.ok);
      expect(await otp.waitForError()).toEqual(error);
      expect(otp.errors).toEqual([error]);
      expect(await otp.trySend("shutdown_test", true)).toEqual({
        ok: false,
        error: {
          t: "vm:exited",
          data: { reason: error.kind, data: error.data },
        },
      });
    }
  });

  // Each process grows its heap with garbage collections, which free the
  // previous heaps. Together, the heaps take several times the memory limit.
  // The emulator args would place carriers in mmap's memory segments.
  test("collected heaps' memory is reused", async ({ otp }) => {
    const { beam } = evalOpts(`
      Grow = fun() -> length(lists:seq(1, 2000000)) end,
      [begin
         {_, Ref} = spawn_monitor(Grow),
         receive {'DOWN', Ref, process, _, normal} -> ok end
       end || _ <- lists:seq(1, 20)],
      wasm:send(#{grown => true}).
    `);
    const boot = await otp.tryBoot({
      beam: {
        ...beam,
        emulatorArgs: ["-Mummc", "1024", "-Musac", "false"],
        maxMemory: { desktop: "256M" },
      },
    });
    assert(boot.ok);

    const grown = otp.waitForEvent("grown");
    expect(await Promise.race([grown, otp.waitForError()])).toEqual({
      grown: true,
    });
  });

  test("spawning OS processes don't kill the VM", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
        Ticker = spawn(fun Tick() ->
          receive
            {ticks, From} -> From ! {ticks, get(ticks)}
          after 20 ->
            put(ticks, case get(ticks) of undefined -> 1; N -> N + 1 end),
            Tick()
          end
        end),

        Fmt = fun(Term) -> iolist_to_binary(io_lib:format("~p", [Term])) end,
        Raises = fun(Fun) ->
          Fmt(try Fun() of Value -> {ok, Value}
              catch Class:Reason -> {Class, Reason}
              end)
        end,

        ok = file:write_file("/tmp/executable", <<"code">>),
        ok = file:change_mode("/tmp/executable", 8#755),

        ok = wasm:send(#{
          lookup => Fmt(inet_db:res_option(lookup)),
          getaddr => Fmt(inet:getaddr("example.com", inet)),
          gethostbyname => Fmt(inet:gethostbyname("example.com")),
          loopback => Fmt(inet:getaddr({127,0,0,1}, inet)),
          cmd => Raises(fun() -> os:cmd("echo hi") end),
          spawn_port => Raises(fun() -> open_port({spawn, "echo hi"}, []) end),
          spawn_executable =>
            Raises(fun() -> open_port({spawn_executable, "/tmp/executable"}, []) end),
          find_executable => os:find_executable("echo"),
          ram_file => element(1, ram_file:open("x", [read, write, ram]))
        }),

        receive after 300 -> ok end,
        Ticker ! {ticks, self()},
        receive {ticks, Ticks} -> ok = wasm:send(#{ticks => Ticks}) end.
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("lookup")).toEqual({
      // `native` would spawn /bin/inet_gethost and halt the VM
      lookup: "[file]",
      getaddr: "{error,nxdomain}",
      gethostbyname: "{error,nxdomain}",
      loopback: "{ok,{127,0,0,1}}",
      cmd: "{error,badarg}",
      spawn_port: "{error,badarg}",
      spawn_executable: "{error,badarg}",
      find_executable: false,
      // A linked-in driver opened with the {spawn, _} tag works
      ram_file: "ok",
    });

    // Other processes keep working, failure would be getting `undefined` atom
    expect(await otp.waitForEvent("ticks")).toEqual({
      ticks: expect.any(Number),
    });
  });
});

test.describe("lifecycle", () => {
  test("TTY output", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const command = async (
        popcorn: {
          onEvent: (handler: (event: unknown) => void) => () => void;
          writeStdin: (chunk: Uint8Array) => void;
        },
        value: number,
      ) =>
        await new Promise<unknown>((resolve) => {
          const unsubscribe = popcorn.onEvent((event) => {
            unsubscribe();
            resolve(event);
          });
          popcorn.writeStdin(new Uint8Array([value]));
        });

      // Default text decodes split UTF-8, uses 80×24 and no shell.
      const stdout: string[] = [];
      const stderr: string[] = [];
      const text = new window.Popcorn({
        workerUrl: "/output-worker.mjs",
        onStdout: (chunk) => stdout.push(chunk),
        onStderr: (chunk) => stderr.push(chunk),
      });
      await text.boot();
      const defaultSize = await command(text, 2);
      const ctrlD = await command(text, 4);
      text.deinit();

      // init infers byte callbacks and forwards custom size; tty keeps the shell.
      const rawStdout: number[][] = [];
      const rawStderr: number[][] = [];
      const init = await window.Popcorn.init({
        workerUrl: "/output-worker.mjs",
        tty: {
          size: { columns: 100, rows: 30 },
          output: "bytes",
        },
        onStdout: (chunk) => rawStdout.push(Array.from(chunk)),
        onStderr: (chunk) => rawStderr.push(Array.from(chunk)),
      });
      const bytes = init;
      const customSize = await command(bytes, 2);
      bytes.deinit();

      // Reboot discards an incomplete UTF-8 sequence.
      const rebootStdout: string[] = [];
      const reboot = new window.Popcorn({
        workerUrl: "/output-worker.mjs",
        onStdout: (chunk) => rebootStdout.push(chunk),
      });
      await reboot.boot();
      rebootStdout.length = 0;
      await command(reboot, 0);
      reboot.deinit();
      await reboot.boot();
      rebootStdout.length = 0;
      await command(reboot, 1);
      reboot.deinit();

      return {
        stdout,
        stderr,
        rawStdout,
        rawStderr,
        defaultSize,
        ctrlD,
        customSize,
        rebootStdout,
      };
    });

    expect(result).toEqual({
      stdout: ["👩", "‍🚀"],
      stderr: ["🚀"],
      rawStdout: [
        [0xf0, 0x9f],
        [0x91, 0xa9, 0xe2, 0x80],
        [0x8d, 0xf0, 0x9f, 0x9a, 0x80],
      ],
      rawStderr: [[0xf0, 0x9f, 0x9a], [0x80]],
      defaultSize: {
        ttySize: { columns: 80, rows: 24 },
        noshell: true,
      },
      ctrlD: { command: 4 },
      customSize: {
        ttySize: { columns: 100, rows: 30 },
        noshell: false,
      },
      rebootStdout: ["👩‍🚀"],
    });
  });

  test("init", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const init = await window.Popcorn.init({});
      init.deinit();
      return { ok: true };
    });

    expect(result).toEqual({ ok: true });
  });

  test("reboot", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const events: unknown[] = [];
      const popcorn = new window.Popcorn({
        beam: {
          extraArgs: ["-eval", "ok = wasm:send(#{ready => true})."],
        },
      });
      popcorn.onEvent((event) => events.push(event));
      let removedEvents = 0;
      const unsubscribe = popcorn.onEvent(() => {
        removedEvents += 1;
      });

      const boot = async () => {
        const ready = new Promise<void>((resolve) => {
          const unsubscribe = popcorn.onEvent((event) => {
            if (
              typeof event === "object" &&
              event !== null &&
              Object.hasOwn(event, "ready")
            ) {
              unsubscribe();
              resolve();
            }
          });
        });
        await popcorn.boot();
        await ready;
        return true;
      };

      const first = await boot();
      const removedAfterFirstBoot = removedEvents;
      unsubscribe();
      popcorn.deinit();
      events.length = 0;
      const second = await boot();
      popcorn.deinit();
      return { first, second, events, removedAfterFirstBoot, removedEvents };
    });

    expect(result.first).toBe(true);
    expect(result.second).toBe(true);
    expect(result.events).toContainEqual({ ready: true });
    expect(result.removedEvents).toBe(result.removedAfterFirstBoot);
  });
});

test.describe("fetch", () => {
  test("requests", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
        {ok, Response} = 'Elixir.Popcorn.Fetch':request(#{
          method => <<"GET">>,
          url => <<"${FETCH_URL}">>
        }),
        #{status := Status, headers := Headers, body := ManifestBody} = Response,
        Manifest = json:decode(ManifestBody),

        Payload = binary:copy(<<255, 0, 65, 254>>, 25000),
        {ok, #{body := EchoBody}} = 'Elixir.Popcorn.Fetch':request(#{
          method => <<"POST">>,
          url => <<"/echo">>,
          body => Payload
        }),

        ok = wasm:send(#{
          status => Status,
          has_vm => is_map_key(<<"vm">>, Manifest),
          header_count => length(Headers),
          size => byte_size(EchoBody),
          identical => EchoBody =:= Payload
        }).
      `),
    );
    assert(boot.ok);

    const event = await otp.waitForEvent("status");
    expect(event).toMatchObject({
      status: 200,
      has_vm: true,
      size: 100000,
      identical: true,
    });
    expect((event as { header_count: number }).header_count).toBeGreaterThan(0);
  });

  // Browsers that send a User-Agent set by the page preflight cross-origin
  // requests that have one, which many servers, like jsDelivr, refuse. Chrome
  // leaves it out, so the test checks the headers Popcorn passes to fetch().
  test("leaves out User-Agent", async ({ otp, page }) => {
    await page.evaluate(() => {
      const fetch = window.fetch;
      const fetched: string[][] = [];
      Object.assign(window, { fetchedHeaders: fetched });
      window.fetch = (input, init) => {
        const names: string[] = [];
        new Headers(init?.headers).forEach((_value, name) => names.push(name));
        fetched.push(names);
        return fetch(input, init);
      };
    });
    const boot = await otp.tryBoot(
      evalOpts(`
        {ok, #{status := Status}} = 'Elixir.Popcorn.Fetch':request(#{
          method => <<"GET">>,
          url => <<"${FETCH_URL}">>,
          headers => [
            {<<"User-Agent">>, <<"popcorn-test">>},
            {<<"x-popcorn-test">>, <<"kept">>}
          ]
        }),
        ok = wasm:send(#{fetched => Status}).
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("fetched")).toEqual({ fetched: 200 });
    const fetched = await page.evaluate(
      () =>
        (window as unknown as { fetchedHeaders: string[][] }).fetchedHeaders,
    );
    expect(fetched).toContainEqual(["x-popcorn-test"]);
  });

  test("blocked request", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
        Result = 'Elixir.Popcorn.Fetch':request(#{
          method => <<"GET">>,
          url => <<"https://example.com/">>
        }),
        {error, {fetch, Message}} = Result,
        ok = wasm:send(#{fetch_error => Message}).
      `),
    );
    assert(boot.ok);

    const event = (await otp.waitForEvent("fetch_error")) as {
      fetch_error: string;
    };
    expect(event.fetch_error).toContain("Failed to fetch");
    expect(event.fetch_error).toContain("CORS");
  });

  test("Req adapter", async ({ otp }) => {
    test.skip(
      process.env.POPCORN_E2E_REQ !== "1",
      "requires OTP assets that provide ssl",
    );

    const boot = await otp.tryBoot(
      evalOpts(`
        {ok, DefaultOptions} = application:get_env(req, default_options),
        'Elixir.Popcorn.Fetch.Req' = proplists:get_value(adapter, DefaultOptions),
        {ok, _} = application:ensure_all_started(req),
        {ok, GetResponse, GetChunks} = 'Elixir.Req':stream(
          <<"/req/get">>,
          [],
          fun(Data, _Response, Chunks) -> {cont, [Data | Chunks]} end,
          [{decode_body, false}]
        ),
        GetStatus = maps:get(status, GetResponse),
        GetHeaders = 'Elixir.Req.Fields':get_list(maps:get(headers, GetResponse)),
        GetBody = iolist_to_binary(lists:reverse(GetChunks)),
        ok = wasm:send(#{
          get_status => GetStatus,
          get_headers => GetHeaders,
          get_body => GetBody
        }),

        Payload = <<"posted through Req">>,
        PostResponse = 'Elixir.Req':'post!'(<<"/req/post">>, [
          {body, Payload},
          {decode_body, false}
        ]),
        PostStatus = maps:get(status, PostResponse),
        PostHeaders = 'Elixir.Req.Fields':get_list(maps:get(headers, PostResponse)),
        PostBody = maps:get(body, PostResponse),
        ok = wasm:send(#{
          post_status => PostStatus,
          post_headers => PostHeaders,
          post_body => PostBody
        }),

        ok = application:stop(popcorn),
        ok = application:set_env(req, default_options, [
          {adapter, 'Elixir.Req.Finch'}
        ]),
        ok = application:start(popcorn),
        {ok, UpdatedOptions} = application:get_env(req, default_options),
        'Elixir.Req.Finch' = proplists:get_value(adapter, UpdatedOptions),

        ok = wasm:send(#{
          adapter_preserved => true
        }).
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("get_status")).toEqual({
      get_status: 206,
      get_headers: expect.arrayContaining([["x-popcorn-fixture", "get"]]),
      get_body: "browser-stream",
    });
    expect(await otp.waitForEvent("post_status")).toEqual({
      post_status: 201,
      post_headers: expect.arrayContaining([["x-popcorn-fixture", "post"]]),
      post_body: "posted through Req",
    });
    expect(await otp.waitForEvent("adapter_preserved")).toEqual({
      adapter_preserved: true,
    });
  });

  test("httpc requests with fetch", async ({ otp }) => {
    const boot = await otp.tryBoot(
      evalOpts(`
        {ok, _} = application:ensure_all_started(inets),
        {ok, {{_, GetStatus, _}, GetHeaders, GetBody}} =
          httpc:request(get, {"/req/get", []}, [], [{body_format, binary}]),
        {ok, {PostStatus, PostBody}} = httpc:request(
          post, {"/req/post", [], "text/plain", "posted through httpc"}, [], [{full_result, false}]
        ),
        ok = wasm:send(#{
          get_status => GetStatus,
          get_fixture => list_to_binary(proplists:get_value("x-popcorn-fixture", GetHeaders)),
          get_body => GetBody,
          post_status => PostStatus,
          post_body => list_to_binary(PostBody)
        }),

        {ok, Ref} = httpc:request(get, {"/req/get", []}, [], [{sync, false}, {stream, {self, once}}]),
        Pid = receive {http, {Ref, stream_start, _Headers, StreamPid}} -> StreamPid end,
        Collect = fun Collect(Parts) ->
          ok = httpc:stream_next(Pid),
          receive
            {http, {Ref, stream, Part}} -> Collect([Part | Parts]);
            {http, {Ref, stream_end, _}} -> iolist_to_binary(lists:reverse(Parts))
          end
        end,
        ok = wasm:send(#{streamed => Collect([])}),

        {ok, _Profile} = inets:start(httpc, [{profile, e2e}]),
        ok = httpc:set_options([{max_sessions, 2}], e2e),
        {ok, [{max_sessions, MaxSessions}]} = httpc:get_options([max_sessions], e2e),
        ok = inets:stop(httpc, e2e),
        ok = wasm:send(#{profile_max_sessions => MaxSessions}).
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("get_status")).toEqual({
      get_status: 206,
      get_fixture: "get",
      get_body: "browser-stream",
      post_status: 201,
      post_body: "posted through httpc",
    });
    expect(await otp.waitForEvent("streamed")).toEqual({
      streamed: "browser-stream",
    });
    expect(await otp.waitForEvent("profile_max_sessions")).toEqual({
      profile_max_sessions: 2,
    });
  });
});

type CacheMode =
  | "available"
  | "absent"
  | "denied"
  | "stalled"
  | "full"
  | "unreadable"
  | "invalid";
type StorageCall = {
  hash: CrossOriginStorageGetFileHandleHash;
  options: CrossOriginStorageGetFileHandleOptions | undefined;
};

const assetTest = test.extend<{
  createAssetEnvironment: typeof installAssetEnvironment;
}>({
  createAssetEnvironment: async ({}, use) => {
    const fetch = globalThis.fetch;
    const navigator = Object.getOwnPropertyDescriptor(globalThis, "navigator");
    try {
      await use(installAssetEnvironment);
    } finally {
      mock.timers.reset();
      globalThis.fetch = fetch;
      if (navigator) Object.defineProperty(globalThis, "navigator", navigator);
      else Reflect.deleteProperty(globalThis, "navigator");
    }
  },
});

assetTest.describe("assets", () => {
  const asset = { url: "https://example.test/app.tar", sha256: "a".repeat(64) };
  const hash = { algorithm: "SHA-256", value: asset.sha256 };
  const contents = new TextEncoder().encode("asset contents");
  const bytes = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

  assetTest("reuse", async ({ createAssetEnvironment }) => {
    const environment = createAssetEnvironment();
    expect(await new Assets().binary(asset)).toEqual(contents);
    await expect.poll(() => environment.files.has(asset.sha256)).toBe(true);

    expect(await new Assets().binary(asset)).toEqual(contents);
    expect(environment.downloads).toEqual([asset.url]);
    expect(environment.files.size).toBe(1);
    expect(environment.calls).toEqual([
      { hash, options: undefined },
      { hash, options: { create: true, origins: "*" } },
      { hash, options: undefined },
    ]);
  });

  assetTest("fallbacks", async ({ createAssetEnvironment }) => {
    assetTest.setTimeout(5_000);
    const cases: {
      name: string;
      mode?: CacheMode;
      enabled?: boolean;
      asset?: { url: string; sha256?: string };
      bypass?: boolean;
    }[] = [
      { name: "unavailable", mode: "absent", bypass: true },
      { name: "disabled", enabled: false, bypass: true },
      { name: "hashless", asset: { url: asset.url }, bypass: true },
      { name: "denied", mode: "denied" },
      { name: "stalled", mode: "stalled" },
      { name: "unreadable", mode: "unreadable" },
    ];

    for (const scenario of cases) {
      await assetTest.step(scenario.name, async () => {
        const environment = createAssetEnvironment(scenario.mode);
        const loader = new Assets(scenario.enabled);
        const result = await loader.binary(scenario.asset ?? asset);

        expect(result).toEqual(contents);
        expect(environment.downloads).toEqual([asset.url]);
        if (scenario.bypass) expect(environment.calls).toEqual([]);
      });
    }

    const invalid = createAssetEnvironment("invalid", bytes);
    await new Assets().wasm(asset.url, asset.sha256);
    expect(invalid.downloads).toEqual([asset.url]);

    const failure = Error("offline");
    globalThis.fetch = async () => {
      throw failure;
    };
    await expect(new Assets().wasm(asset.url, asset.sha256)).rejects.toBe(
      failure,
    );

    const extension = createAssetEnvironment("absent", bytes);
    const channel = new MessageChannel();
    let discarded = false;
    channel.port2.onmessage = ({ data }) => {
      if (data.type === "lookup") data.reply.close();
      else {
        void data.body.cancel();
        discarded = true;
      }
    };
    try {
      expect(await new Assets(true, channel.port1).binary(asset)).toEqual(
        bytes,
      );
      await expect.poll(() => discarded).toBe(true);
      expect(extension.downloads).toEqual([asset.url]);
    } finally {
      channel.port1.close();
      channel.port2.close();
    }
  });

  assetTest("write failure", async ({ createAssetEnvironment }) => {
    const environment = createAssetEnvironment("full");
    const result = await new Assets().binary(asset);
    await expect.poll(() => environment.rejectedWrites).toBe(1);

    expect(result).toEqual(contents);
    expect(environment.downloads).toEqual([asset.url]);
    expect(environment.files.size).toBe(0);
  });

  assetTest("write timeout", async ({ createAssetEnvironment }) => {
    for (const stage of ["open", "write"] as const) {
      await assetTest.step(stage, async () => {
        createAssetEnvironment();
        const write = installStalledWrite(stage, contents);
        mock.timers.enable({ apis: ["setTimeout"] });
        try {
          expect(await new Assets().binary(asset)).toEqual(contents);
          await write.started;
          mock.timers.tick(30_000);
          expect(write.state.canceled).toBe(true);
          write.resume();
          mock.timers.reset();
          await expect.poll(() => write.state.aborted).toBe(true);
        } finally {
          write.resume();
          mock.timers.reset();
        }
      });
    }
  });

  assetTest("relay disposal", async ({ createAssetEnvironment }) => {
    createAssetEnvironment();
    const write = installStalledWrite("write", contents);
    const relay = Assets.extension();
    assert(relay);
    Reflect.deleteProperty(navigator, "crossOriginStorage");
    try {
      expect(await new Assets(true, relay.port).binary(asset)).toEqual(
        contents,
      );
      await write.started;
      relay.close();
      await expect.poll(() => write.state.canceled).toBe(true);
      write.resume();
      await expect.poll(() => write.state.aborted).toBe(true);
    } finally {
      write.resume();
      relay.close();
    }
  });
});

function installStalledWrite(
  stage: "open" | "write",
  bytes: Uint8Array<ArrayBuffer>,
) {
  let resume!: () => void;
  let start!: () => void;
  const blocked = new Promise<void>((resolve) => {
    resume = resolve;
  });
  const started = new Promise<void>((resolve) => {
    start = resolve;
  });
  const state = { canceled: false, aborted: false };
  const body = new ReadableStream({
    start(controller) {
      controller.enqueue(bytes);
    },
    cancel() {
      state.canceled = true;
    },
  });
  const writable = new WritableStream({
    write() {
      if (stage === "write") {
        start();
        return blocked;
      }
    },
    abort() {
      state.aborted = true;
    },
  });
  installStorage(async (_hash, options) => {
    if (!options?.create) throw Error("miss");
    return {
      createWritable: async () => {
        if (stage === "open") {
          start();
          await blocked;
        }
        return writable;
      },
    };
  });
  globalThis.fetch = async () => {
    const response = new Response(bytes);
    response.clone = () => new Response(body);
    return response;
  };
  return { state, started, resume };
}

function installAssetEnvironment(
  mode: CacheMode = "available",
  body: BodyInit = "asset contents",
) {
  const files = new Map<string, File>();
  const calls: StorageCall[] = [];
  const downloads: Parameters<typeof fetch>[0][] = [];
  const environment = { files, calls, downloads, rejectedWrites: 0 };
  const storage = {
    async getFileHandle(
      hash: CrossOriginStorageGetFileHandleHash,
      options?: CrossOriginStorageGetFileHandleOptions,
    ) {
      calls.push({ hash, options });
      if (options?.create) {
        if (mode === "full") {
          environment.rejectedWrites++;
          throw new DOMException("full", "QuotaExceededError");
        }
        return {
          createWritable: async () => {
            const chunks: Uint8Array<ArrayBuffer>[] = [];
            return new WritableStream<Uint8Array<ArrayBuffer>>({
              write: (chunk) => {
                chunks.push(chunk);
              },
              close: () => {
                files.set(hash.value, new File(chunks, hash.value));
              },
            });
          },
        };
      }
      if (mode === "unreadable") {
        const file = new File([], hash.value);
        file.stream = () =>
          new ReadableStream({
            start(controller) {
              controller.error(Error("disk read failed"));
            },
          });
        return { getFile: async () => file };
      }
      if (mode === "invalid")
        return { getFile: async () => new File(["invalid wasm"], hash.value) };
      if (mode === "denied")
        throw new DOMException("denied", "NotAllowedError");
      if (mode === "stalled") return new Promise(() => {});
      const file = files.get(hash.value);
      if (file === undefined) throw new DOMException("miss", "NotFoundError");
      return { getFile: async () => file };
    },
  };
  installStorage(storage.getFileHandle);
  if (mode === "absent")
    Reflect.deleteProperty(navigator, "crossOriginStorage");
  globalThis.fetch = async (url) => {
    downloads.push(url);
    return new Response(body);
  };
  return environment;
}

function installStorage(
  getFileHandle: (
    ...args: Parameters<CrossOriginStorageManager["getFileHandle"]>
  ) => Promise<unknown>,
) {
  Object.defineProperty(globalThis, "navigator", {
    configurable: true,
    value: { crossOriginStorage: { getFileHandle } },
  });
}

async function installAssetCache(
  page: Page,
  scope: "worker" | "page" = "worker",
) {
  if (scope === "page") await page.evaluate(installBrowserStorage);
  else
    await page.route("**/worker.mjs*", async (route) => {
      const response = await route.fetch();
      const source = await response.text();
      await route.fulfill({
        response,
        body: `(${installBrowserStorage.toString()})();\n${source}`,
      });
    });
  return {
    hashes: () =>
      page.evaluate(async () => {
        // TODO: Update the cache name whenever the COS-cached assets
        // change and delete the old cache and create a new one.
        const cache = await caches.open("popcorn-e2e-cos");
        const keys = await cache.keys();
        return keys
          .map((key) => new URL(key.url).pathname.split("/").pop())
          .sort();
      }),
  };
}

function installBrowserStorage() {
  Object.defineProperty(navigator, "crossOriginStorage", {
    configurable: true,
    value: {
      async getFileHandle(
        hash: CrossOriginStorageGetFileHandleHash,
        options?: CrossOriginStorageGetFileHandleOptions,
      ) {
        const cache = await caches.open("popcorn-e2e-cos");
        const url = new URL(`/cos-cache/${hash.value}`, location.origin).href;
        if (!options?.create) {
          const response = await cache.match(url);
          if (response === undefined)
            throw new DOMException("miss", "NotFoundError");
          const file = new File([await response.blob()], hash.value);
          return { getFile: async () => file };
        }
        return {
          createWritable: async () => {
            const chunks: Uint8Array<ArrayBuffer>[] = [];
            return new WritableStream<Uint8Array<ArrayBuffer>>({
              write: (chunk) => {
                chunks.push(chunk);
              },
              close: () => cache.put(url, new Response(new Blob(chunks))),
            });
          },
        };
      },
    },
  });
}

async function assetHashes(page: Page) {
  const response = await page.request.get(FETCH_URL);
  assert(response.ok());
  const manifest: {
    vm: { sha256: string };
    bundles: Record<string, { sha256: string }>;
  } = await response.json();
  const hashes = [
    manifest.vm.sha256,
    ...Object.values(manifest.bundles).map((bundle) => bundle.sha256),
  ];
  assert(hashes.every((hash) => typeof hash === "string"));
  return [...new Set(hashes)].sort();
}
