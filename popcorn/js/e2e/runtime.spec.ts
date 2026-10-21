import { schedulers, type PopcornOpts } from "@swmansion/popcorn";
import { assert, evalOpts, expect, test } from "./helpers";
import { PopcornError, type SerializedError } from "../src/errors";

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

    expect(() => PopcornError.deserialize({ t: "timeout", data: {} }))
      .toThrow(Error);
  });
});

test.describe("boot", () => {
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
        message: "maxMemory must be a multiple of 64 KiB between 64 MiB and 2 GiB",
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

    const diagnostic = expect.stringMatching(/Kernel pid terminated.*missing_user/s);
    expect(boot).toEqual({
      ok: false,
      error: { t: "vm:exited", data: { reason: "error", data: diagnostic } },
    });
    expect(otp.errors).toEqual([{ kind: "error", data: diagnostic }]);
  });

  test("schedulers", async ({ otp }) => {
    const boot = await otp.tryBoot({
      beam: {
        emulatorArgs: schedulers({ base: 2, dirtyCpu: 2, dirtyIo: 2 }),
        extraArgs: [
          "-eval",
          `
            ok = wasm:send(#{
              schedulers => erlang:system_info(schedulers),
              dirty_cpu_schedulers => erlang:system_info(dirty_cpu_schedulers),
              dirty_io_schedulers => erlang:system_info(dirty_io_schedulers)
            }).
          `,
        ],
      },
    });
    assert(boot.ok);

    expect(await otp.waitForEvent("schedulers")).toEqual({
      schedulers: 2,
      dirty_cpu_schedulers: 2,
      dirty_io_schedulers: 2,
    });
  });

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

    for (const kind of ["manifest-entry", "boot-script", "tarball"]) {
      await page.route("**/asset-errors/**", (route) => {
        if (route.request().url().endsWith("manifest.json")) {
          return route.fulfill({
            json: {
              vm: { boot: "start.boot", config: "sys.config" },
              apps: kind === "manifest-entry"
                ? {}
                : {
                    kernel: { bundle: "core" },
                    stdlib: { bundle: "core" },
                    compiler: { bundle: "core" },
                  },
              bundles: { core: { tar: "kernel.tar" } },
            },
          });
        }
        if (route.request().url().endsWith("sys.config")) {
          return route.fulfill({ body: "[]." });
        }
        if (kind === "tarball" && route.request().url().endsWith("start.boot")) {
          return route.fulfill({ body: "" });
        }
        return route.fulfill({ status: 404 });
      });
      const asset = await createOtp();
      const result = await asset.tryBoot({
        beam: { otpAssetsRoot: "/asset-errors/" },
      });
      const data = kind === "manifest-entry"
        ? {
            kind,
            url: "/asset-errors/manifest.json",
            name: "kernel",
            available: [],
          }
        : {
            kind,
            url: kind === "boot-script"
              ? "/asset-errors/start.boot"
              : "/asset-errors/kernel.tar",
          };
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
    const failure = { t: "vm:exited", data: { reason: "error", data: diagnostic } };
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
      const boot = await otp.tryBoot({ workerUrl, timeoutsMs: { boot: 30_000 } });
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
      const boot = await otp.tryBoot(evalOpts(`
        true = register(shutdown_test, self()),
        wasm:send(#{shutdown_ready => true}),
        receive {wasm, _} -> ${expression} end.
      `));
      assert(boot.ok);
      await otp.waitForEvent("shutdown_ready");

      const sent = await otp.trySend("shutdown_test", true);
      assert(sent.ok);
      expect(await otp.waitForError()).toEqual(error);
      expect(otp.errors).toEqual([error]);
      expect(await otp.trySend("shutdown_test", true)).toEqual({
        ok: false,
        error: { t: "vm:exited", data: { reason: error.kind, data: error.data } },
      });
    }
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
        'Elixir.Popcorn.Fetch' = proplists:get_value(adapter, DefaultOptions),
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
});
