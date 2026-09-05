# Send messages and use GenServers

Popcorn supports three JavaScript-to-BEAM message styles. Choose the style that
matches the target process API.

## Call a GenServer

Use a call when JavaScript needs a reply. A call also gives the sender
back-pressure.

```typescript
const result = await popcorn.genserver.call("counter", ["add", 1]);
if (!result.ok) throw result.error;

console.log(result.data);
```

Add `Popcorn.Proxy` to the application supervision tree before you use calls.
The target process receives a normal `handle_call/3` callback.

A call timeout does not cancel work in the GenServer.

## Cast to a GenServer

Use a cast when JavaScript does not need a reply:

```typescript
const result = await popcorn.genserver.cast("counter", "reset");
if (!result.ok) throw result.error;
```

A successful result confirms delivery to the proxy. It does not confirm that
the target handled the cast.

## Send a process message

Use `send` for a regular process mailbox:

```typescript
const result = await popcorn.send("worker", { task: "refresh" });
if (!result.ok) throw result.error;
```

The process receives this message:

```elixir
{:wasm, %{"task" => "refresh"}}
```

Use `Popcorn.Wasm.is_message/1` in a guard, or match `{:wasm, payload}`
directly.

## Publish an event to JavaScript

Register the handler before boot if the application publishes startup events:

```typescript
const popcorn = new Popcorn();
const removeHandler = popcorn.onEvent((event) => console.log(event));

const result = await popcorn.boot();
if (!result.ok) throw result.error;
```

Publish an event from Elixir:

```elixir
Popcorn.Wasm.send(%{event: "ready", worker: inspect(self())})
```

Messages with no JavaScript handlers are lost. Call `removeHandler()` when the
page no longer needs the subscription.

## Handle bridge errors

Bridge operations return a result object. Use `error.t` when code must handle a
specific error. Do not match the human-readable message.

```typescript
const result = await popcorn.genserver.call("counter", "value");

if (!result.ok && result.error.t === "genserver:noproc") {
  console.error("The counter is not running");
}
```
