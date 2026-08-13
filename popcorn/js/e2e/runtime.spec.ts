import { schedulers } from "@swmansion/popcorn";
import { assert, evalOpts, expect, test } from "./helpers";

const FETCH_URL = "/assets/otp/manifest.json";

test.describe("boot", () => {
  test("apps and eval", async ({ otp }) => {
    const boot = await otp.boot(
      evalOpts(`
        {ok, _} = application:ensure_all_started(elixir),
        {ok, _} = application:ensure_all_started(logger),
        3 = element(1, 'Elixir.Code':eval_string(<<"1 + 2">>)),
        _ = 'Elixir.Logger':level(),
        ok = wasm:send(#{runtime_ready => true}).
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("runtime_ready")).toEqual({
      runtime_ready: true,
    });
  });

  test("startup bridge", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const events: unknown[] = [];
      let booted = false;
      const popcorn = new window.Popcorn({
        beam: {
          env: { POPCORN_STARTUP_EVENT: "bridge" },
        },
      });
      popcorn.onEvent((event) => events.push({ event, beforeBoot: !booted }));

      const boot = await popcorn.boot();
      booted = true;
      popcorn.deinit();
      return {
        boot: boot.ok,
        events,
      };
    });

    expect(result).toEqual({
      boot: true,
      events: expect.arrayContaining([
        { event: { startup_send: true }, beforeBoot: true },
        { event: { startup_action: true }, beforeBoot: true },
        { event: { startup_run_js: 42 }, beforeBoot: true },
      ]),
    });
  });

  test("no entrypoint", async ({ page }) => {
    await page.route("/assets/otp/manifest.json", async (route) => {
      const response = await route.fetch();
      const manifest = (await response.json()) as Record<string, unknown>;
      await route.fulfill({ response, json: { ...manifest, entrypoint: null } });
    });

    const result = await page.evaluate(async () => {
      const popcorn = new window.Popcorn({
        beam: {},
      });
      const boot = await popcorn.boot();
      popcorn.deinit();
      return boot.ok;
    });

    expect(result).toBe(true);
  });

  test("entrypoint failure", async ({ createOtp }) => {
    const otp = await createOtp();
    const boot = await otp.boot({
      beam: {
        env: { POPCORN_STARTUP_EVENT: "fail" },
      },
    });

    expect(boot).toEqual({
      ok: false,
      error: { t: "vm:exited", data: { reason: "exit", data: 1 } },
    });
  });

  test("schedulers", async ({ otp }) => {
    const boot = await otp.boot({
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
    const timeout = await timedOut.boot({
      beam: {},
      timeoutsMs: { boot: 0 },
    });
    expect(timeout).toEqual({
      ok: false,
      error: { t: "timeout:init", data: { timeoutMs: 0 } },
    });

    const missing = await page.evaluate(async () => {
      const errors: unknown[] = [];
      const popcorn = new window.Popcorn({
        beam: { otpAssetsRoot: "/missing/otp/" },
        onError: (event) => errors.push(event),
      });
      const result = await popcorn.boot();
      if (result.ok) {
        return { errors, boot: { ok: true, data: null } };
      }
      return {
        errors,
        boot: { ok: false, error: result.error.serialize() },
      };
    });
    expect(missing).toEqual({
      errors: [],
      boot: {
        ok: false,
        error: {
          t: "beam:missing-manifest",
          data: { url: "/missing/otp/manifest.json" },
        },
      },
    });
  });

  test("worker exit", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const popcorn = new window.Popcorn({
        beam: {},
        workerUrl: "/fault-worker.mjs",
        timeoutsMs: { boot: 30_000 },
      });
      const startMs = performance.now();
      const boot = await popcorn.boot();
      const elapsedMs = performance.now() - startMs;
      if (boot.ok) {
        return { elapsedMs, boot: { ok: true, data: null } };
      }
      return {
        elapsedMs,
        boot: { ok: false, error: boot.error.serialize() },
      };
    });

    expect(result.boot).toEqual({
      ok: false,
      error: { t: "vm:exited", data: { reason: "abort", data: "boom" } },
    });
    expect(result.elapsedMs).toBeLessThan(5_000);
  });
});

test.describe("runtime", () => {
  test("spawning OS processes don't kill the VM", async ({ otp }) => {
    const boot = await otp.boot(
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
          writeStdin: (chunk: Uint8Array) => { ok: boolean };
        },
        value: number,
      ) =>
        await new Promise<unknown>((resolve) => {
          const unsubscribe = popcorn.onEvent((event) => {
            unsubscribe();
            resolve(event);
          });
          const write = popcorn.writeStdin(new Uint8Array([value]));
          if (!write.ok) throw new Error("stdin command failed");
        });

      // Default text decodes split UTF-8 and uses 80×24.
      const stdout: string[] = [];
      const stderr: string[] = [];
      const text = new window.Popcorn({
        beam: {},
        workerUrl: "/output-worker.mjs",
        onStdout: (chunk) => stdout.push(chunk),
        onStderr: (chunk) => stderr.push(chunk),
      });
      await text.boot();
      const defaultSize = await command(text, 2);
      const ctrlD = await command(text, 4);
      text.deinit();

      // init infers byte callbacks and forwards custom size.
      const rawStdout: number[][] = [];
      const rawStderr: number[][] = [];
      const init = await window.Popcorn.init({
        beam: {},
        workerUrl: "/output-worker.mjs",
        tty: {
          size: { columns: 100, rows: 30 },
          output: "bytes",
        },
        onStdout: (chunk) => rawStdout.push(Array.from(chunk)),
        onStderr: (chunk) => rawStderr.push(Array.from(chunk)),
      });
      if (!init.ok) throw init.error;
      const bytes = init.data;
      const customSize = await command(bytes, 2);
      bytes.deinit();

      // Reboot discards an incomplete UTF-8 sequence.
      const rebootStdout: string[] = [];
      const reboot = new window.Popcorn({
        beam: {},
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
      defaultSize: { ttySize: { columns: 80, rows: 24 } },
      ctrlD: { command: 4 },
      customSize: { ttySize: { columns: 100, rows: 30 } },
      rebootStdout: ["👩‍🚀"],
    });
  });

  test("init", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const init = await window.Popcorn.init({
        beam: {},
      });
      if (!init.ok) return { ok: false, error: init.error.serialize() };
      init.data.deinit();
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
        const result = await popcorn.boot();
        await ready;
        return result.ok;
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
    const boot = await otp.boot(
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
    const boot = await otp.boot(
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

    const boot = await otp.boot(
      evalOpts(`
        {ok, DefaultOptions} = application:get_env(req, default_options),
        'Elixir.Popcorn.Fetch' = proplists:get_value(adapter, DefaultOptions),
        {ok, _} = application:ensure_all_started(req),
        Response = 'Elixir.Req':'get!'(<<"${FETCH_URL}">>, [
          {decode_body, true}
        ]),
        Status = maps:get(status, Response),
        Body = maps:get(body, Response),

        ok = application:stop(popcorn),
        ok = application:set_env(req, default_options, [
          {adapter, 'Elixir.Req.Finch'}
        ]),
        ok = application:start(popcorn),
        {ok, UpdatedOptions} = application:get_env(req, default_options),
        'Elixir.Req.Finch' = proplists:get_value(adapter, UpdatedOptions),

        ok = wasm:send(#{
          status => Status,
          has_vm => is_map_key(<<"vm">>, Body),
          adapter_preserved => true
        }).
      `),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("status")).toEqual({
      status: 200,
      has_vm: true,
      adapter_preserved: true,
    });
  });
});
