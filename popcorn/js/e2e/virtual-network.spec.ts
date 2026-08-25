import { assert, evalOpts, expect, test } from "./helpers";

test.describe("virtual network", () => {
  test("JavaScript TCP", async ({ createOtp, page }) => {
    const server = await createOtp();
    const serverBoot = await server.boot(
      evalOpts(`
        spawn(fun() ->
          {ok, Listener} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}]),
          {ok, Port} = inet:port(Listener),
          ok = wasm:send(#{js_tcp_port => Port}),
          {ok, Socket} = gen_tcp:accept(Listener, 5000),
          {ok, <<"abcdef">>} = gen_tcp:recv(Socket, 6, 5000),
          ok = gen_tcp:send(Socket, <<"one">>),
          ok = gen_tcp:send(Socket, <<"two">>),
          {error, closed} = gen_tcp:recv(Socket, 0, 5000),
          ok = wasm:send(#{js_tcp_done => true})
        end).
      `),
    );
    assert(serverBoot.ok);
    const event = (await server.waitForEvent("js_tcp_port")) as { js_tcp_port: number };

    const result = await page.evaluate(async (port) => {
      const socket = await window.Popcorn.connect("vm-1", port);
      await socket.write(new TextEncoder().encode("ab"));
      await socket.write(new TextEncoder().encode("cdef"));
      const reader = socket.readable.getReader();
      let reply = "";
      while (reply.length < 6) {
        const { value } = await reader.read();
        reply += new TextDecoder().decode(value);
      }
      socket.closeWrite();
      return { reply, localAddress: socket.localAddress };
    }, event.js_tcp_port);

    expect(result).toEqual({ reply: "onetwo", localAddress: "10.255.0.1" });
    expect(await server.waitForEvent("js_tcp_done")).toEqual({ js_tcp_done: true });
  });

  test("TCP and UDP between VMs", async ({ createOtp }) => {
    const server = await createOtp();
    const serverBoot = await server.boot(
      evalOpts(`
        spawn(fun() ->
          {ok, Listener} = gen_tcp:listen(0, [binary, {active, false}, {packet, 2}]),
          {ok, TcpPort} = inet:port(Listener),
          {ok, Udp} = gen_udp:open(0, [binary, {active, false}]),
          {ok, UdpPort} = inet:port(Udp),
          spawn(fun() ->
            {ok, Socket} = gen_tcp:accept(Listener, 5000),
            {ok, <<"hello">>} = gen_tcp:recv(Socket, 0, 5000),
            ok = gen_tcp:send(Socket, <<"world">>),
            ok = inet:setopts(Socket, [{active, once}]),
            receive
              {tcp, Socket, <<"once">>} -> ok = wasm:send(#{tcp_once => true})
            after 5000 -> error(tcp_once_timeout)
            end
          end),
          UdpWorker = spawn(fun() ->
            receive {socket, Socket} ->
              {ok, {Address, Port, <<>>}} = gen_udp:recv(Socket, 0, 5000),
              ok = gen_udp:send(Socket, Address, Port, <<"reply">>)
            end
          end),
          ok = gen_udp:controlling_process(Udp, UdpWorker),
          UdpWorker ! {socket, Udp},
          ok = wasm:send(#{ports => #{tcp => TcpPort, udp => UdpPort}}),
          receive stop -> ok end
        end).
      `),
    );
    assert(serverBoot.ok);
    const ports = (await server.waitForEvent("ports")) as {
      ports: { tcp: number; udp: number };
    };

    const client = await createOtp();
    const clientBoot = await client.boot(
      evalOpts(`
        {ok, Socket} = gen_tcp:connect("vm-1", ${ports.ports.tcp},
                                      [binary, {active, false}, {packet, 2}], 5000),
        ok = gen_tcp:send(Socket, <<"hello">>),
        {ok, <<"world">>} = gen_tcp:recv(Socket, 0, 5000),
        ok = gen_tcp:send(Socket, <<"once">>),
        {ok, Udp} = gen_udp:open(0, [binary, {active, false}]),
        ok = gen_udp:send(Udp, "vm-1", ${ports.ports.udp}, <<>>),
        {ok, {_Address, _Port, <<"reply">>}} = gen_udp:recv(Udp, 0, 5000),
        ok = wasm:send(#{network => #{tcp => <<"world">>, udp => <<"reply">>}}).
      `),
    );
    assert(clientBoot.ok);

    expect(await client.waitForEvent("network")).toEqual({
      network: { tcp: "world", udp: "reply" },
    });
    expect(await server.waitForEvent("tcp_once")).toEqual({ tcp_once: true });
  });

  test("timeouts, stream boundaries, ownership, and close", async ({
    createOtp,
  }) => {
    const server = await createOtp();
    const serverBoot = await server.boot(
      evalOpts(`
        spawn(fun() ->
          {ok, Listener} = gen_tcp:listen(0, [binary, {active, false}, {packet, raw}]),
          {ok, Port} = inet:port(Listener),
          {error, timeout} = gen_tcp:accept(Listener, 10),
          ok = wasm:send(#{accept_timeout => Port}),
          {ok, Socket} = gen_tcp:accept(Listener, 5000),
          NewOwner = spawn(fun() ->
            receive {socket, Owned} ->
              {ok, <<"abc">>} = gen_tcp:recv(Owned, 3, 5000),
              {ok, <<"def">>} = gen_tcp:recv(Owned, 3, 5000),
              ok = gen_tcp:send(Owned, <<"ack">>),
              {error, closed} = gen_tcp:recv(Owned, 0, 5000),
              ok = wasm:send(#{stream => true, peer_closed => true})
            end
          end),
          ok = gen_tcp:controlling_process(Socket, NewOwner),
          NewOwner ! {socket, Socket},
          receive stop -> ok end
        end).
      `),
    );
    assert(serverBoot.ok);
    const timeout = (await server.waitForEvent("accept_timeout")) as {
      accept_timeout: number;
    };

    const client = await createOtp();
    const clientBoot = await client.boot(
      evalOpts(`
        {ok, Socket} = gen_tcp:connect("vm-1", ${timeout.accept_timeout},
                                      [binary, {active, false}, {packet, raw}], 5000),
        ok = gen_tcp:send(Socket, <<"ab">>),
        ok = gen_tcp:send(Socket, <<"cdef">>),
        {ok, <<"ack">>} = gen_tcp:recv(Socket, 3, 5000),
        ok = gen_tcp:close(Socket),
        ok = wasm:send(#{client_closed => true}).
      `),
    );
    assert(clientBoot.ok);

    expect(await client.waitForEvent("client_closed")).toEqual({
      client_closed: true,
    });
    expect(await server.waitForEvent("stream")).toEqual({
      stream: true,
      peer_closed: true,
    });
  });
});
