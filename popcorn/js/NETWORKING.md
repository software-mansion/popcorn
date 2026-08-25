# Browser-local TCP and UDP

The OTP browser runtime provides a virtual IPv4 network shared by every
`Popcorn` VM created in the same page. Applications use the normal `gen_tcp`,
`gen_udp`, and `inet` APIs. No application adapter or JavaScript callback is
required.

Each VM receives a stable address in the `10.0.0.0/8` range and a name such as
`vm-1` for its lifetime. `localhost` and `127.0.0.1` refer to the calling VM.
Binding `0.0.0.0` binds the VM's assigned address. Port `0` allocates an
ephemeral port; use `inet:port/1` to read it.

Supported TCP operations are `listen`, `accept`, `connect`, `send`, `recv`,
`close`, and `controlling_process`. Supported receive options are
`{active, false | true | once}`, `binary` or `list`, and
`{packet, raw | 1 | 2 | 4}`. `inet:setopts/2`, `inet:getopts/2`,
`inet:sockname/1`, `inet:peername/1`, and `inet:port/1` work for this option
set. Unsupported options and operations return `{error, enotsup}`.

Supported UDP operations are `open`, `send`, `recv`, `close`, `connect`, and
`controlling_process`. Active and passive delivery preserve datagram
boundaries, including zero-length datagrams.

TCP is an ordered byte stream. A receive can combine several sends or consume
part of one send. UDP always delivers one complete datagram. Broker queues are
bounded to 1 MiB. An oversized UDP send returns `{error, enobufs}`; a TCP queue
overflow closes the connection with `enobufs`.

The network is local to one browser page. It does not provide Internet access,
TLS, IPv6, DNS, multicast, broadcast, SCTP, or operating-system file
descriptors. It is also not a security boundary: any script that can access the
page can create a VM or interact with the page-local broker.

JavaScript can open a stream socket with `Popcorn.connect(host, port, options)`.
The returned socket exposes a byte `ReadableStream`, `write`, `closeWrite`, and
`close`. Passing an `AbortSignal` cancels a pending connection. Reads preserve
byte order but not write boundaries, and the same 1 MiB broker limit applies.

`Popcorn.fetch(input, init)` routes `http://vm-N/...` and virtual `10.x.x.x`
addresses through virtual TCP. Other URLs use the browser's native `fetch`.
Virtual requests return standard streaming `Response` objects, follow redirects
unless configured otherwise, and support buffered request bodies. Connections
currently send `Connection: close` and are not reused. HTTPS is unsupported.

The TCP compatibility surface supports Ranch and cleartext Cowboy listeners,
including concurrent acceptors, integer active mode, `tcp_passive`, standard
Ranch listener options, ownership transfer, and graceful write shutdown. Cowboy
TLS listeners remain unsupported.

## Example

One VM can listen with ordinary OTP calls:

```erlang
{ok, Listener} = gen_tcp:listen(0, [binary, {active, false}, {packet, 2}]),
{ok, Port} = inet:port(Listener),
{ok, Socket} = gen_tcp:accept(Listener),
{ok, Message} = gen_tcp:recv(Socket, 0),
ok = gen_tcp:send(Socket, Message).
```

Another VM in the page can connect by virtual host name:

```erlang
{ok, Socket} = gen_tcp:connect("vm-1", Port,
                               [binary, {active, false}, {packet, 2}]),
ok = gen_tcp:send(Socket, <<"hello">>),
{ok, <<"hello">>} = gen_tcp:recv(Socket, 0).
```
