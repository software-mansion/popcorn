import type { DistEndpoint, Popcorn } from "@swmansion/popcorn";
import {
  assert,
  expect,
  test,
  type DistTransportName,
  type OtpFactory,
} from "./helpers";

// Popcorn's VM uses its popcorn_dist carrier. The nodes share a cookie.
const distOpts = (code: string, transport: DistTransportName = "relay") => ({
  beam: { extraArgs: ["-setcookie", "popcorn", "-eval", code.trim()] },
  transport,
});

// Becomes the node and runs the rest of the code, which receives the test's
// messages. rex serves rpc calls, which kernel starts only with the
// distribution cooking option, which this package doesn't have.
const node = (name: string, code: string) => `
  true = register(dist_test, self()),
  {ok, _} = rpc:start_link(),
  {ok, _} = net_kernel:start('${name}', #{name_domain => longnames}),
  ${code}
`;

// a connects to b, exchanges messages with it, then sees it disconnect
async function connectAndDisconnect(
  createOtp: OtpFactory,
  transport: DistTransportName,
): Promise<void> {
  const [a, b] = await Promise.all([createOtp(), createOtp()]);

  const bootB = await b.tryBoot(
    distOpts(
      node(
        "b@popcorn",
        `
        Echo = spawn(fun Loop() ->
                         receive {From, Msg} -> From ! {echo, node(), Msg}, Loop() end
                     end),
        true = register(echo, Echo),
        ok = wasm:send(#{node_started => node()}),
        receive {wasm, <<"disconnect">>} -> ok end,
        true = erlang:disconnect_node('a@popcorn'),
        receive stop -> ok end.
        `,
      ),
      transport,
    ),
  );
  assert(bootB.ok);

  const bootA = await a.tryBoot(
    distOpts(
      node(
        "a@popcorn",
        `
        ok = wasm:send(#{node_started => node()}),
        receive {wasm, <<"connect">>} -> ok end,
        true = net_kernel:connect_node('b@popcorn'),
        ok = net_kernel:monitor_nodes(true),
        {echo, 'b@popcorn'} ! {self(), hello},
        Echo = receive {echo, From, Msg} -> [From, Msg] after 10000 -> timeout end,
        Rpc = rpc:call('b@popcorn', erlang, node, []),
        ok = wasm:send(#{connected => #{nodes => nodes(), echo => Echo, rpc => Rpc}}),
        Down = receive {nodedown, Node} -> Node after 10000 -> timeout end,
        ok = wasm:send(#{down => #{node => Down, nodes => nodes()}}),
        receive stop -> ok end.
        `,
      ),
      transport,
    ),
  );
  assert(bootA.ok);

  expect(await a.waitForEvent("node_started")).toEqual({
    node_started: "a@popcorn",
  });
  expect(await b.waitForEvent("node_started")).toEqual({
    node_started: "b@popcorn",
  });

  assert((await a.trySend("dist_test", "connect")).ok);
  expect(await a.waitForEvent("connected")).toEqual({
    connected: {
      nodes: ["b@popcorn"],
      echo: ["b@popcorn", "hello"],
      rpc: "b@popcorn",
    },
  });

  assert((await b.trySend("dist_test", "disconnect")).ok);
  expect(await a.waitForEvent("down")).toEqual({
    down: { node: "b@popcorn", nodes: [] },
  });
}

test.describe("distribution", () => {
  test("two instances in one page", async ({ createOtp }) => {
    await connectAndDisconnect(createOtp, "relay");
  });

  test("two instances through a custom transport", async ({ createOtp }) => {
    await connectAndDisconnect(createOtp, "channel");
  });

  // Rather than when the connection setup times out, after 7 seconds
  test("the relay refuses missing nodes at once", async ({ otp }) => {
    const boot = await otp.tryBoot(
      distOpts(
        node(
          "a@popcorn",
          `
          Started = erlang:monotonic_time(millisecond),
          Connected = net_kernel:connect_node('c@popcorn'),
          Ms = erlang:monotonic_time(millisecond) - Started,
          ok = wasm:send(#{missing => #{connected => Connected, fast => Ms < 3000}}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(boot.ok);

    expect(await otp.waitForEvent("missing")).toEqual({
      missing: { connected: false, fast: true },
    });
  });

  // Rather than when b's ticks stop, after 45 to 75 seconds
  test("the relay disconnects the peers of an instance that shuts down", async ({
    createOtp,
  }) => {
    const [a, b] = await Promise.all([createOtp(), createOtp()]);
    const bootB = await b.tryBoot(
      distOpts(
        node(
          "b@popcorn",
          `
          ok = wasm:send(#{node_started => node()}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(bootB.ok);
    const bootA = await a.tryBoot(
      distOpts(
        node(
          "a@popcorn",
          `
          ok = wasm:send(#{node_started => node()}),
          receive {wasm, <<"connect">>} -> ok end,
          true = net_kernel:connect_node('b@popcorn'),
          ok = net_kernel:monitor_nodes(true),
          ok = wasm:send(#{connected => nodes()}),
          Down = receive {nodedown, Node} -> Node after 10000 -> timeout end,
          ok = wasm:send(#{down => Down}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(bootA.ok);

    await Promise.all([
      a.waitForEvent("node_started"),
      b.waitForEvent("node_started"),
    ]);
    assert((await a.trySend("dist_test", "connect")).ok);
    expect(await a.waitForEvent("connected")).toEqual({
      connected: ["b@popcorn"],
    });

    await b.deinit();
    expect(await a.waitForEvent("down")).toEqual({ down: "b@popcorn" });
  });

  // a only connects out, and is hidden, as nodes that don't listen are. It's
  // announced with its first connection, so that its VM refuses connections
  // at once, rather than when their setup times out, and the relay
  // disconnects its peers when its instance shuts down.
  test("a node that doesn't listen", async ({ createOtp }) => {
    const [a, b] = await Promise.all([createOtp(), createOtp()]);
    const bootB = await b.tryBoot(
      distOpts(
        node(
          "b@popcorn",
          `
          ok = wasm:send(#{node_started => node()}),
          receive {wasm, <<"connect">>} -> ok end,
          Started = erlang:monotonic_time(millisecond),
          Connected = net_kernel:connect_node('a@popcorn'),
          Ms = erlang:monotonic_time(millisecond) - Started,
          ok = wasm:send(#{refused => #{connected => Connected, fast => Ms < 3000}}),
          receive {wasm, <<"monitor">>} -> ok end,
          ok = net_kernel:monitor_nodes(true, [{node_type, all}]),
          ok = wasm:send(#{monitoring => nodes(connected)}),
          Down = receive {nodedown, Node, _} -> Node after 10000 -> timeout end,
          ok = wasm:send(#{down => Down}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(bootB.ok);
    const bootA = await a.tryBoot(
      distOpts(`
        true = register(dist_test, self()),
        Opts = #{name_domain => longnames, dist_listen => false},
        {ok, _} = net_kernel:start('a@popcorn', Opts),
        ok = wasm:send(#{node_started => node()}),
        receive {wasm, <<"connect">>} -> ok end,
        true = net_kernel:connect_node('b@popcorn'),
        ok = wasm:send(#{connected => nodes(connected)}),
        receive {wasm, <<"disconnect">>} -> ok end,
        true = erlang:disconnect_node('b@popcorn'),
        ok = wasm:send(#{disconnected => nodes(connected)}),
        receive {wasm, <<"connect">>} -> ok end,
        true = net_kernel:connect_node('b@popcorn'),
        ok = wasm:send(#{reconnected => nodes(connected)}),
        receive stop -> ok end.
      `),
    );
    assert(bootA.ok);

    await Promise.all([
      a.waitForEvent("node_started"),
      b.waitForEvent("node_started"),
    ]);
    assert((await a.trySend("dist_test", "connect")).ok);
    expect(await a.waitForEvent("connected")).toEqual({
      connected: ["b@popcorn"],
    });
    assert((await a.trySend("dist_test", "disconnect")).ok);
    expect(await a.waitForEvent("disconnected")).toEqual({ disconnected: [] });

    assert((await b.trySend("dist_test", "connect")).ok);
    expect(await b.waitForEvent("refused")).toEqual({
      refused: { connected: false, fast: true },
    });

    assert((await a.trySend("dist_test", "connect")).ok);
    expect(await a.waitForEvent("reconnected")).toEqual({
      reconnected: ["b@popcorn"],
    });
    assert((await b.trySend("dist_test", "monitor")).ok);
    expect(await b.waitForEvent("monitoring")).toEqual({
      monitoring: ["a@popcorn"],
    });

    await a.deinit();
    expect(await b.waitForEvent("down")).toEqual({ down: "a@popcorn" });
  });

  // Each boot gets a new endpoint, and the relay connects the rebooted node
  // like a new one
  test("the relay connects a node again after its instance reboots", async ({
    page,
  }) => {
    const result = await page.evaluate(
      async ({ aCode, bCode }) => {
        const relay = window.popcorn.createDistributionRelay();
        const bEndpoints: DistEndpoint[] = [];
        const opts = (code: string) => ({
          beam: { extraArgs: ["-setcookie", "popcorn", "-eval", code] },
        });

        const waitFor = (popcorn: Popcorn, key: string) =>
          new Promise<unknown>((resolve) => {
            const unsubscribe = popcorn.onEvent((event) => {
              if (typeof event === "object" && event !== null && key in event) {
                unsubscribe();
                resolve((event as Record<string, unknown>)[key]);
              }
            });
          });
        const boot = async (popcorn: Popcorn) => {
          const ready = waitFor(popcorn, "ready");
          await popcorn.boot();
          await ready;
        };
        const ping = async () => {
          const pong = waitFor(a, "pong");
          await a.send("dist_test", "ping");
          return await pong;
        };

        const b = new window.Popcorn({
          ...opts(bCode),
          distribution: (endpoint) => {
            bEndpoints.push(endpoint);
            return relay(endpoint);
          },
        });
        const a = new window.Popcorn({ ...opts(aCode), distribution: relay });
        await boot(b);
        await boot(a);
        const before = await ping();

        b.deinit();
        await boot(b);
        const after = await ping();
        a.deinit();
        b.deinit();

        return {
          before,
          after,
          endpoints: bEndpoints.length,
          distinct: bEndpoints[0] !== bEndpoints[1],
        };
      },
      {
        aCode: `
          true = register(dist_test, self()),
          {ok, _} = net_kernel:start('a@popcorn', #{name_domain => longnames}),
          ok = wasm:send(#{ready => node()}),
          Loop = fun Loop() ->
                     receive {wasm, <<"ping">>} -> ok end,
                     true = net_kernel:connect_node('b@popcorn'),
                     {echo, 'b@popcorn'} ! {self(), hello},
                     Echo = receive {echo, From, Msg} -> [From, Msg] after 10000 -> timeout end,
                     ok = wasm:send(#{pong => Echo}),
                     Loop()
                 end,
          Loop().
        `,
        bCode: `
          {ok, _} = net_kernel:start('b@popcorn', #{name_domain => longnames}),
          Echo = spawn(fun Loop() ->
                           receive {From, Msg} -> From ! {echo, node(), Msg}, Loop() end
                       end),
          true = register(echo, Echo),
          ok = wasm:send(#{ready => node()}),
          receive stop -> ok end.
        `,
      },
    );

    expect(result).toEqual({
      before: ["b@popcorn", "hello"],
      after: ["b@popcorn", "hello"],
      endpoints: 2,
      distinct: true,
    });
  });

  // net_kernel doesn't tell the carrier when a node that doesn't listen
  // stops, so the carrier watches net_kernel. The relay then forgets the
  // node, and refuses connections to it at once, rather than when their setup
  // times out.
  test("a node that doesn't listen stops", async ({ createOtp }) => {
    const [a, b] = await Promise.all([createOtp(), createOtp()]);
    const bootB = await b.tryBoot(
      distOpts(
        node(
          "b@popcorn",
          `
          ok = wasm:send(#{node_started => node()}),
          receive {wasm, <<"connect">>} -> ok end,
          Started = erlang:monotonic_time(millisecond),
          Connected = net_kernel:connect_node('a@popcorn'),
          Ms = erlang:monotonic_time(millisecond) - Started,
          ok = wasm:send(#{refused => #{connected => Connected, fast => Ms < 3000}}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(bootB.ok);
    const bootA = await a.tryBoot(
      distOpts(`
        true = register(dist_test, self()),
        Opts = #{name_domain => longnames, dist_listen => false},
        {ok, _} = net_kernel:start('a@popcorn', Opts),
        ok = wasm:send(#{ready => node()}),
        receive {wasm, <<"connect">>} -> ok end,
        true = net_kernel:connect_node('b@popcorn'),
        ok = net_kernel:stop(),
        ok = wasm:send(#{stopped => true}),
        receive stop -> ok end.
      `),
    );
    assert(bootA.ok);

    await Promise.all([
      a.waitForEvent("ready"),
      b.waitForEvent("node_started"),
    ]);
    assert((await a.trySend("dist_test", "connect")).ok);
    expect(await a.waitForEvent("stopped")).toEqual({ stopped: true });
    assert((await b.trySend("dist_test", "connect")).ok);
    expect(await b.waitForEvent("refused")).toEqual({
      refused: { connected: false, fast: true },
    });
  });

  // a gets its name from its first connection, and is announced only then,
  // so that its next connection doesn't announce a new name, which would
  // make the relay disconnect the first one
  test("a node with a dynamic name", async ({ createOtp }) => {
    const [a, b, c] = await Promise.all([
      createOtp(),
      createOtp(),
      createOtp(),
    ]);
    const echo = `
      Echo = spawn(fun Loop() ->
                       receive {From, Msg} -> From ! {echo, node(), Msg}, Loop() end
                   end),
      true = register(echo, Echo),
      ok = wasm:send(#{node_started => node()}),
    `;
    const bootB = await b.tryBoot(
      distOpts(
        node(
          "b@popcorn",
          `${echo}
          receive {wasm, <<"check">>} -> ok end,
          ok = wasm:send(#{peers => length(nodes(connected))}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(bootB.ok);
    const bootC = await c.tryBoot(
      distOpts(node("c@popcorn", `${echo} receive stop -> ok end.`)),
    );
    assert(bootC.ok);
    const bootA = await a.tryBoot(
      distOpts(`
        true = register(dist_test, self()),
        Opts = #{name_domain => longnames, dist_listen => false},
        {ok, _} = net_kernel:start('undefined@popcorn', Opts),
        ok = wasm:send(#{dynamic => node()}),
        receive {wasm, <<"connect">>} -> ok end,
        true = net_kernel:connect_node('b@popcorn'),
        true = net_kernel:connect_node('c@popcorn'),
        timer:sleep(500),
        {echo, 'b@popcorn'} ! {self(), hello},
        Echo = receive {echo, From, Msg} -> [From, Msg] after 5000 -> timeout end,
        Named = lists:suffix("@popcorn", atom_to_list(node()))
                    andalso node() =/= 'undefined@popcorn',
        ok = wasm:send(#{connected => #{named => Named, echo => Echo,
                                        nodes => lists:sort(nodes(connected))}}),
        receive stop -> ok end.
      `),
    );
    assert(bootA.ok);

    await Promise.all([
      a.waitForEvent("dynamic"),
      b.waitForEvent("node_started"),
      c.waitForEvent("node_started"),
    ]);
    assert((await a.trySend("dist_test", "connect")).ok);
    expect(await a.waitForEvent("connected")).toEqual({
      connected: {
        named: true,
        echo: ["b@popcorn", "hello"],
        nodes: ["b@popcorn", "c@popcorn"],
      },
    });
    assert((await b.trySend("dist_test", "check")).ok);
    expect(await b.waitForEvent("peers")).toEqual({ peers: 1 });
  });

  // A transport that throws fails the boot when it's set up, and can't stop
  // the shutdown when it's told that the node stopped
  test("a transport that throws", async ({ page }) => {
    const result = await page.evaluate(async () => {
      const failing = new window.Popcorn({
        distribution: () => {
          throw new Error("no transport");
        },
      });
      const bootError = await failing.boot().then(
        () => null,
        (error: { serialize(): unknown }) => error.serialize(),
      );
      const sendAfterBoot = await failing.trySend("dist_test");

      let events = 0;
      const popcorn = new window.Popcorn({
        beam: {
          extraArgs: [
            "-eval",
            `{ok, _} = net_kernel:start('a@popcorn', #{name_domain => longnames}),
             Loop = fun L() -> ok = wasm:send(#{tick => true}), timer:sleep(50), L() end,
             Loop().`,
          ],
        },
        distribution: () => (event) => {
          if (event.type === "node_stopped") throw new Error("handler failed");
        },
        onEvent: () => {
          events += 1;
        },
      });
      await popcorn.boot();
      await new Promise((resolve) => setTimeout(resolve, 500));
      let thrown = null;
      try {
        popcorn.deinit();
      } catch (error) {
        thrown = String(error);
      }
      const eventsAtDeinit = events;
      await new Promise((resolve) => setTimeout(resolve, 500));
      const sendAfterDeinit = await popcorn.trySend("dist_test");

      return {
        bootError,
        sendAfterBoot: sendAfterBoot.ok,
        thrown,
        eventsAfterDeinit: events - eventsAtDeinit,
        sendAfterDeinit: sendAfterDeinit.ok,
      };
    });

    expect(result).toEqual({
      bootError: {
        t: "vm:exited",
        data: { reason: "error", data: "Error: no transport" },
      },
      sendAfterBoot: false,
      thrown: "Error: handler failed",
      eventsAfterDeinit: 0,
      sendAfterDeinit: false,
    });
  });

  // b becomes a node as kernel starts, and its entrypoint application connects
  // to a, which is only possible if b receives distribution messages while it
  // boots
  test("a node that connects while it boots", async ({ createOtp }) => {
    const [a, b] = await Promise.all([createOtp(), createOtp()]);
    const bootA = await a.tryBoot(
      distOpts(
        node(
          "a@popcorn",
          `
          ok = wasm:send(#{node_started => node()}),
          receive stop -> ok end.
          `,
        ),
      ),
    );
    assert(bootA.ok);
    await a.waitForEvent("node_started");

    const bootB = await b.tryBoot({
      beam: {
        extraArgs: [
          ...["-name", "b@popcorn", "-setcookie", "popcorn"],
          ...["-kernel", "start_distribution", "true"],
        ],
        env: { POPCORN_STARTUP_EVENT: "dist_connect" },
      },
      transport: "relay",
    });
    assert(bootB.ok);
    expect(await b.waitForEvent("startup_connected")).toEqual({
      startup_connected: true,
    });
  });
});
