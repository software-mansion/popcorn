# Build an interactive counter

This tutorial adds a counter to the `Foo` application from
[Installation](installation.html). You will start with one GenServer call, then
add casts, events, process identifiers, and runtime cleanup.

Each section solves a limitation in the previous version. Run the page after
each section to see why the next Popcorn feature is useful.

## Request an updated value

The page needs a count that survives between button clicks. Keep that state in
a GenServer and ask it to increment the value.

### Add the counter

Create `lib/foo/counter.ex`:

```elixir
defmodule Foo.Counter do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, 0, name: :counter)
  end

  @impl true
  def init(count), do: {:ok, count}

  @impl true
  def handle_call(["increment", amount], _from, count) do
    count = count + amount
    {:reply, count, count}
  end
end
```

The process owns one integer. It is registered as `:counter`, which lets
JavaScript address it as `"counter"`.

The request pattern is an Elixir list containing a binary string. JavaScript
arrays become lists, and JavaScript strings become binaries when they cross the
Popcorn bridge.

### Supervise the counter and proxy

Replace the children in `lib/foo/application.ex`:

```elixir
@impl true
def start(_type, _args) do
  children = [
    Popcorn.Proxy,
    Foo.Counter
  ]

  Supervisor.start_link(children,
    strategy: :one_for_one,
    name: Foo.Supervisor
  )
end
```

`Popcorn.Proxy` turns JavaScript calls and casts into standard GenServer
messages. The supervisor starts it before the counter.

### Add one button

Create `priv/static/index.html`:

```html
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <title>Popcorn counter</title>
  </head>
  <body>
    <h1>Counter: <output id="count">0</output></h1>
    <button id="increment" type="button" disabled>Increment</button>
    <p id="reply" aria-live="polite"></p>

    <script type="module" src="./index.js"></script>
  </body>
</html>
```

Create `priv/static/index.js`:

```javascript
import { Popcorn } from "./popcorn/index.mjs";

const increment = document.querySelector("#increment");
const countNode = document.querySelector("#count");
const replyNode = document.querySelector("#reply");

const boot = await Popcorn.init();
if (!boot.ok) throw boot.error;

const popcorn = boot.data;
increment.disabled = false;

increment.addEventListener("click", async () => {
  const result = await popcorn.genserver.call("counter", ["increment", 1]);
  if (!result.ok) throw result.error;

  countNode.textContent = result.data;
  replyNode.textContent = `The call returned ${result.data}.`;
});
```

`genserver.call()` waits for `handle_call/3` to reply. Its result contains the
new count, so the page can render it immediately. A call also provides
back-pressure: the click handler waits while the counter handles the request.

Start the page:

```sh
mix popcorn.dev
```

Open [localhost:8000](http://localhost:8000). Each click should increment the
count and show the returned value.

## Reset without waiting

The reset action does not need to return data. A cast sends work to a GenServer
without waiting for its callback to finish.

Add this callback to `Foo.Counter`:

```elixir
@impl true
def handle_cast("reset", _count) do
  {:noreply, 0}
end
```

Add a second button after **Increment**:

```html
<button id="reset" type="button" disabled>Reset</button>
```

Get the new button with the other document elements, then enable it after
Popcorn boots:

```js
const reset = document.querySelector("#reset");

// After a successful boot:
reset.disabled = false;
```

Then add its click handler:

```js
reset.addEventListener("click", async () => {
  const result = await popcorn.genserver.cast("counter", "reset");
  if (!result.ok) throw result.error;

  replyNode.textContent = "The cast reached Popcorn.Proxy.";
});
```

Try the new button. The process resets, but the number on the page does not
change. A successful cast confirms delivery to `Popcorn.Proxy`; it does not
return the new GenServer state.

Click **Increment** once. The call returns `1`, which proves the reset happened.
The stale display is the next problem to solve.

## Publish every state change

The browser needs updates even when an operation has no reply. Publish an event
whenever the counter changes.

Add the alias and `publish/1` helper to `Foo.Counter`:

```elixir
alias Popcorn.Wasm

defp publish(count) do
  Wasm.send(%{event: :counter_updated, count: count, counter: self()})
end
```

Publish the initial value and each change:

```diff
 @impl true
-def init(count), do: {:ok, count}
+def init(count) do
+  publish(count)
+  {:ok, count}
+end

 @impl true
 def handle_call(["increment", amount], _from, count) do
   count = count + amount
+  publish(count)
   {:reply, count, count}
 end

 @impl true
 def handle_cast("reset", _count) do
+  publish(0)
   {:noreply, 0}
 end
```

The first event is sent while the supervision tree starts. `Popcorn.init()`
registers no event handler until after startup, so that event would be lost.
Replace the initialization code with three explicit operations:

```diff
-const boot = await Popcorn.init();
-if (!boot.ok) throw boot.error;
-
-const popcorn = boot.data;
+const popcorn = new Popcorn();
+let counter;
+
+const removeEventHandler = popcorn.onEvent((message) => {
+  if (message.event !== "counter_updated") return;
+
+  counter = message.counter;
+  countNode.textContent = message.count;
+});
+
+const boot = await popcorn.boot();
+if (!boot.ok) throw boot.error;
```

Keep the existing lines that enable both buttons after `boot()` succeeds.

The event handler is now ready before the VM starts. It receives the initial
count as well as every later update.

The event also contains `self()`. Popcorn turns that process ID (PID) into an
opaque JavaScript value. Use it as the target in both handlers:

```diff
-const result = await popcorn.genserver.call("counter", ["increment", 1]);
+const result = await popcorn.genserver.call(counter, ["increment", 1]);
```

```diff
-const result = await popcorn.genserver.cast("counter", "reset");
+const result = await popcorn.genserver.cast(counter, "reset");
```

Remove the direct count update from the increment handler because events now
own the visible count:

```diff
-countNode.textContent = result.data;
```

Reload the page. The startup event displays `0`. Both **Increment** and
**Reset** now update the count through the same event path.

## Stop the runtime with the page

The page now owns an event subscription and a VM worker. Release both when the
page goes away:

```js
addEventListener(
  "pagehide",
  () => {
    removeEventHandler();
    popcorn.deinit();
  },
  { once: true },
);
```

`onEvent()` returns the unsubscribe function. `deinit()` stops the worker and
releases resources tracked by this Popcorn instance.

## Choose the right operation

| Operation          | Use it when                                  | Completion means                       |
| ------------------ | -------------------------------------------- | -------------------------------------- |
| `genserver.call()` | JavaScript needs a reply or back-pressure    | The GenServer replied                  |
| `genserver.cast()` | JavaScript does not need a reply             | The proxy received the cast            |
| `onEvent()`        | JavaScript needs changes published by Elixir | The handler received a published value |

See [JavaScript and Elixir communication](js-communication.html) for `run_js`,
plain process messages, tracked browser values, timeouts, and bridge errors.
The [HTTP](http.html) and [Terminal](terminal.html) guides cover browser-specific
APIs that do not fit this counter.
