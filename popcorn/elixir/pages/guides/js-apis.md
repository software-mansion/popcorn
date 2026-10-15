# Communicate with JavaScript

Popcorn has multiple ways to communicate with Elixir from JavaScript.

> #### Philosophy {: .neutral}
>
> The main philosophy of the API is that exceptions aren't meant to be caught — exceptions signify misconfiguration, mistake in using the APIs, or bug in the Popcorn.
>
> For errors that can be handled, we return `{ok: true | false, data, error}` result object.
> API methods throw or reject on failure. Only `send`, `call`, and `cast` have `try` prefixed variants (`trySend`, `tryCall`, and `tryCast`) that return result objects. [List of errors](`t:JSLib.popcorn_errors/0`).
>
> Use former if you don't expect errors and latter if you can handle them in some way (e.g. retry the operation on timeout).

## JavaScript

### Initialization

In JavaScript, we have two ways of creating Popcorn instance. First is [`async Popcorn.init(opts)`](`JSLib.Popcorn.init/1`), which sets up initial state and boots a VM.

This can be split into [`const popcorn = new Popcorn(opts)`](JSLib.Popcorn.html#Popcorn/1) and [`async popcorn.boot()`](`JSLib.Popcorn.boot/0`). It's rare to do that, it's mostly needed if you need to do some setup before booting the VM.

List of options, including VM ones is in [`PopcornOpts`](`t:JSLib.popcorn_opts/0`).

### IO

To handle input and output, we have:

- `onStdout(text)`
- `onStderr(text)`
- `writeStdin(text)`

First two can be set when constructing the instance via [`Popcorn.init()`](`JSLib.Popcorn.init/1`) or replaced on instance anytime.

See ["Interacting with terminal"](terminal.md) to learn more.

### Sending and receiving events

Elixir can send events to the JavaScript via `Popcorn.Wasm.send/1`. To receive them, we need to register [`onEvent(event)`](`JSLib.Popcorn.onEvent/1`) handler.

We can also send events back using [`async popcorn.send(pid, payload)`](`JSLib.Popcorn.send/2`). `pid` can be either a named process or PID handle. The PID handle can be obtained by either sending it from Elixir in an event or passing it through `Popcorn.Wasm.run_js/3`. Elixir process receives them in the mailbox, in `{:wasm, payload}` shape.

Additionally, we have [`async popcorn.cast(pid, payload, opts)`](`JSLib.Popcorn.cast/3`) and [`async popcorn.call(pid, payload, opts)`](`JSLib.Popcorn.call/3`). They're used to communicate with GenServers.
All `cast()`s and `call()`s are going through internal `Popcorn.Proxy` which keeps tabs of timeouts and routes the messages to correct processes.

Messages sent to the Elixir will convert the JavaScript types to the Elixir ones. Popcorn provides [`atom()`](`JSLib.atom/1`) (`a()` alias) and [`tuple()`](`JSLib.tuple/3`) (`t()` alias) helpers to create native Elixir types.

## Elixir

### Initialization

You can interact with JavaScript after it finished initialization. After OTP application supervision tree finishes, the JavaScript may be still initializing. Use `Popcorn.Wasm.await_ready/0` to explicitly wait for it to finish or:

- synchronize by sending an event from JavaScript
- register handlers and wait for user interaction

### Sending events to JavaScript

Use `Popcorn.Wasm.send/1` to send the events from the Elixir to JavaScript. They will be processed in [`onEvent(event)`](`JSLib.Popcorn.onEvent/1`) handler. All values will be serialized and deserialized back, check ["Sharing values across languages"](concepts/values.md) to see supported list of types.

### Running JS scripts in Elixir

`Popcorn.Wasm.run_js/3` and `Popcorn.Wasm.run_js!/3` can be used to do that. First argument is a string with JavaScript source, second is a map of arguments passed to that source.

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
|> Popcorn.Wasm.run_js!(%{node: node, pid: self(), event: event})
```

You can see that JavaScript source is a function that takes two arguments and returns a value.
The first argument is the deserialized map of arguments and second is a map of actions you can do from that function:

- `cast()`
- `call()`
- `send()`

Actions perform the same thing as [`async popcorn.send(pid, payload)`](`JSLib.Popcorn.send/2`) and others, mentioned earlier.

> One thing to be mindful of is to not `call()` the process that runs `run_js/3`. It will deadlock.

The return value will be returned back to the Elixir (subject to [value conversions](concepts/values.md)).

### Sending requests

See ["Making HTTP requests"](http.md).
