# Communicate with JavaScript

Popcorn has multiple ways to communicate between Elixir and JavaScript.

> #### Philosophy {: .neutral}
>
> The main philosophy of the API is that exceptions aren't meant to be caught — invalid usage, misconfiguration, and bugs in Popcorn should fail loudly.
>
> For errors that can be handled, the `try` variants return a result object: `{ok: true, data}` or `{ok: false, error}`.
> API methods throw or reject on failure. Only `send`, `call`, and `cast` have `try`-prefixed variants (`trySend`, `tryCall`, and `tryCast`) that return result objects. [List of errors](`t:JSLib.popcorn_errors/0`).
>
> Use the throwing methods if you don't expect errors and the `try` variants if you can handle them in some way (e.g. retry the operation on timeout).

## JavaScript

### Initialization

In JavaScript, we have two ways of creating a Popcorn instance. The first is [`async Popcorn.init(opts)`](`JSLib.Popcorn.init/1`), which sets up initial state and boots a VM.

This can be split into [`const popcorn = new Popcorn(opts)`](JSLib.Popcorn.html#Popcorn/1) and [`async popcorn.boot()`](`JSLib.Popcorn.boot/0`). It's rare to do that; it's mostly needed if you need to do some setup before booting the VM.

The list of options, including VM options, is in [`PopcornOpts`](`t:JSLib.popcorn_opts/0`).

### IO

To handle input and output, we have:

- `onStdout(text)`
- `onStderr(text)`
- `writeStdin(text)`

The first two can be set when constructing the instance via [`Popcorn.init()`](`JSLib.Popcorn.init/1`) or replaced on the instance at any time.

See ["Interacting with terminal"](terminal.md) to learn more.

### Sending and receiving events

Elixir can send events to JavaScript via `Popcorn.JS.send/1`. To receive them, we need to register an [`onEvent(handler)`](`JSLib.Popcorn.onEvent/1`) callback.

We can also send events back using [`async popcorn.send(pid, payload)`](`JSLib.Popcorn.send/2`). `pid` can be either a registered process name or a PID handle. The PID handle can be obtained by either sending it from Elixir in an event or passing it through `Popcorn.JS.run/3`. The Elixir process receives them in the mailbox, in the form `{:wasm, payload}`.

Additionally, we have [`async popcorn.cast(pid, payload, opts)`](`JSLib.Popcorn.cast/3`) and [`async popcorn.call(pid, payload, opts)`](`JSLib.Popcorn.call/3`). They're used to communicate with GenServers.
All `cast()` and `call()` requests go through the internal `Popcorn.Proxy` which keeps tabs of timeouts and routes the messages to the correct processes.

Messages sent to Elixir have their JavaScript types converted to Elixir types. Popcorn provides [`atom()`](`JSLib.atom/1`) (`a()` alias) and [`tuple()`](`JSLib.tuple/3`) (`t()` alias) helpers to create native Elixir types.

## Elixir

### Initialization

You can interact with JavaScript after it has finished initialization. After the OTP application supervision tree starts, the JavaScript may still be initializing. Use `Popcorn.JS.await_ready/1` to explicitly wait for it to finish or:

- synchronize by sending an event from JavaScript
- register handlers and wait for user interaction

### Sending events to JavaScript

Use `Popcorn.JS.send/1` to send events from Elixir to JavaScript. They will be processed in an [`onEvent(handler)`](`JSLib.Popcorn.onEvent/1`) callback. All values are serialized and deserialized; check ["Sharing values across languages"](values.md) to see the supported types.

### Running JS scripts in Elixir

`Popcorn.JS.run/3` and `Popcorn.JS.run!/3` can be used to do that. The first argument is a string with JavaScript source, and the second is a map of arguments passed to that source.

Example:

```elixir
"""
(args, actions) => {
  const { node, pid, event } = args;
  const { cast } = actions;

  node.onclick = () => cast(pid, event);

  return pid;
}
"""
|> Popcorn.JS.run!(%{node: node, pid: self(), event: event})
```

You can see that JavaScript source is a function that takes two arguments and returns a value.
The first argument is the deserialized map of arguments and the second is a map of actions you can perform from that function:

- `cast()`
- `call()`
- `send()`

These actions perform the same operations as [`async popcorn.send(pid, payload)`](`JSLib.Popcorn.send/2`) and others, mentioned earlier.

> One thing to be mindful of is to not `call()` the process that runs `run/3`. It will deadlock.

The return value will be returned to Elixir (subject to [value conversions](values.md)).

### Sending requests

See ["Making HTTP requests"](http.md).
