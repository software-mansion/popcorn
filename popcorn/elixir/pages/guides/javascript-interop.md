## Run JavaScript from Elixir

`Popcorn.Wasm.run_js/3` runs a JavaScript function in the browser page. The
calling BEAM process waits for the result.

### Return a value

```elixir
{:ok, 1.0} =
  Popcorn.Wasm.run_js(
    """
    ({ x }) => Math.round(x)
    """,
    %{x: 0.6}
  )
```

The function receives the argument map as its first argument. Popcorn converts
the returned value to a BEAM term.

## Use arguments

Pass data separately from the function source:

```elixir
Popcorn.Wasm.run_js!(
  """
  ({id, text}) => {
    const statusNode = document.querySelector(id);
    statusNode.textContent = text;
  }
  """,
  %{id: "#status", text: "Ready"}
)
```

Do not build JavaScript source with string interpolation. Separate arguments
avoid quoting errors and code injection.

## Call back into BEAM

The second function argument contains bridge actions:

````elixir
Popcorn.Wasm.run_js!(
  """
  ({target}, {send}) => {
    const refreshNode = document.querySelector("#refresh");

    refreshNode.addEventListener("click", () => {
      void send(target, {event: "refresh"});
+++++++ lrllvxrr a3ecab59 (rebased revision)
> #### Warning {: .warning}
>
> Do not build JavaScript source with string interpolation. Separate arguments
> avoid quoting errors and code injection.

### Call back into BEAM

The second function argument contains actions you can take to communicate with the VM:

```elixir
Popcorn.Wasm.run_js!(
 """
 ({ target }, { send }) => {
   const refresh = document.querySelector("#refresh");

   refresh.addEventListener("click", () => {
     send(target, {event: "refresh"});
   });
 }
 """,
 %{target: self()}
)
````

The BEAM process receives `{:wasm, %{"event" => "refresh"}}`.

The action object also provides `call` and `cast`. Those actions require a
running `Popcorn.Proxy`.

### Keep a JavaScript object

Return a `t:Popcorn.Wasm.html.tracked_value/0` for a DOM node or another object:

```elixir
element =
  Popcorn.Wasm.run_js!(
    """
    () => {
      const chart = document.querySelector("#chart");

      return new TrackedValue(chart);
    }
    """,
    %{}
  )

Popcorn.Wasm.run_js!(
  "({ element }) => element.replaceChildren()",
  %{element: element}
)
```

Add an idempotent cleanup function when the object owns a listener, timer, or
other resource.

### Avoid deadlocks

`run_js/3` blocks only the calling BEAM process. Other processes continue to
run.

If you use `call` to message the process running `run_js/3`, it will deadlock.

The current bridge evaluates JavaScript source. The page Content Security
Policy (CSP) must permit `unsafe-eval`.

## Publish events from Elixir to JavaScript

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

## Communicate with Elixir from JavaScript

Popcorn supports three ways to communicate with Elixir:

- `call(process, payload)`
- `cast(process, payload)`
- `send(process, payload)`

### Call a GenServer

Use a call when JavaScript needs a reply. A call also gives the sender
back-pressure.

```typescript
const result = await popcorn.genserver.call("counter", ["add", 1]);
if (!result.ok) throw result.error;

console.log(result.data);
```

Add `Popcorn.Proxy` to the application supervision tree before you use calls.
Target process processes the message in a `handle_call/3` callback.

A call timeout does not cancel work in the GenServer.

### Cast to a GenServer

Use a cast when JavaScript does not need a reply:

```typescript
const result = await popcorn.genserver.cast("counter", "reset");
if (!result.ok) throw result.error;
```

A successful result confirms delivery to the proxy. It does not confirm that
the target handled the cast.

### Send a process message

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

First, attach a handler for events in JavaScript:

```typescript
const { ok, data: popcorn, error } = await Popcorn.init();
if (!ok) throw error;

const _removeHandler = popcorn.onEvent((event) => console.log(event));
```

and publish an event from Elixir:

```elixir
Popcorn.Wasm.send(%{event: "ready", worker: inspect(self())})
```

Messages with no JavaScript handlers are lost. Call `removeHandler()` (`onEvent` returns an unsubscribe function) when handler is no longer needed.

> Popcorn supports booting in two phases, if VM sends events during startup:
>
> ```typescript
> const popcorn = new Popcorn();
> const removeHandler = popcorn.onEvent((event) => {
>   /* ... */
> });
>
> const { ok, data: popcorn, error } = await popcorn.boot();
> if (!ok) throw error;
> ```

### Handle bridge errors

Bridge operations return a result object. Use `error.t` when code must handle a
specific error.

```typescript
const result = await popcorn.genserver.call("counter", "value");

if (!result.ok && result.error.t === "genserver:noproc") {
  console.error("The counter is not running");
}
```
