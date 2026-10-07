# Migrate from Popcorn 0.3

Popcorn 0.4 replaces AtomVM with Erlang/OTP compiled to WebAssembly.
This change removes the AtomVM bundle and changes both bridge APIs.

Use this guide to migrate an application from Popcorn 0.3.
The guide uses Vite, but the Rollup and esbuild plugins use the same options.

## Review the main changes

| Popcorn 0.3 | Popcorn 0.4 |
| --- | --- |
| AtomVM runs in a hidden iframe. | The Erlang/OTP BEAM runs in a Web Worker. |
| `mix popcorn.cook` creates one `.avm` bundle. | The bundler plugin packages compiled applications as runtime assets. |
| The application calls <code>Popcorn.Wasm.ready/1</code>. | OTP starts the application through its normal application callback. |
| JavaScript calls a custom mailbox protocol. | JavaScript can use normal GenServer calls and casts. |
| The bridge uses JSON values. | The bridge uses Erlang external term format (ETF) values. |
| JavaScript methods can throw or return mixed result shapes. | Bridge operations return typed result objects. |
| Browser code uses iframe-specific helpers. | Browser code runs in the page and uses explicit bridge actions. |

The new runtime supports more OTP behavior than AtomVM.
It still operates inside the browser sandbox.

## Update the packages and toolchain

Replace the Popcorn dependency in `mix.exs`:

```elixir
defp deps do
  [
    {:popcorn, "0.4.0-next.2"}
  ]
end
```

Install the matching JavaScript prerelease:

```console
npm install @swmansion/popcorn@next
```

The Elixir and JavaScript package versions must match.
Popcorn 0.4 requires Elixir 1.19 or later.

Use the toolchain from the selected Popcorn release.
The packager checks the host OTP version against the WebAssembly runtime.

Then get the dependencies and compile the application:

```console
mix deps.get
mix compile
```

## Use normal OTP application startup

Remove these calls from application startup:

- <code>Popcorn.Wasm.ready/0</code>
- <code>Popcorn.Wasm.ready/1</code>
- <code>Popcorn.Wasm.set_default_receiver/1</code>

Popcorn 0.4 starts the application from its `.app` specification.
`Popcorn.init()` waits for the application callback and supervision tree.

Keep the application callback in `mix.exs`:

```elixir
def application do
  [
    extra_applications: [:logger],
    mod: {MyApp.Application, []}
  ]
end
```

Start application processes with a supervisor:

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Popcorn.Proxy,
      MyApp.Worker
    ]

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: MyApp.Supervisor
    )
  end
end
```

Add `Popcorn.Proxy` only when JavaScript uses GenServer calls or casts.
The proxy has the registered name `:popcorn_proxy` by default.

## Replace `.avm` bundle creation

Delete the Popcorn 0.3 output configuration:

```elixir
config :popcorn, out_dir: "dist/wasm"
```

Also remove `mix popcorn.cook` from build scripts.
Popcorn 0.4 does not create or load `.avm` files.

Configure the bundler plugin with the Mix project and OTP application:

```typescript
import { defineConfig } from "vite";
import { popcorn } from "@swmansion/popcorn/vite";

export default defineConfig({
  plugins: [
    popcorn({
      rootDir: "../",
    }),
  ],
});
```

Set `rootDir` to the Mix project directory.
The plugin uses the current Mix application by default. Set `app` only to
select another application.

The plugin runs `mix popcorn.cook`, which compiles the project before packaging
the active Mix environment and target.
It packages the application, its dependencies, the boot file, and the runtime manifest.

Remove the old `treeshake` and `extra_beams` build options.
The new `strip` option removes nonessential BEAM chunks but does not remove functions.

Use `extraApps` for applications that dependency analysis cannot find:

```typescript
popcorn({
  rootDir: "../",
  extraApps: ["eex"],
});
```

Use `app: null` when the runtime must not start an application.
See [Package an application](packaging.html) for all plugin options.

Popcorn 0.4 removes these Mix tasks:

- `mix popcorn.cook`
- `mix popcorn.gen.js`
- `mix popcorn.build_runtime`
- `mix popcorn.server`

Use the selected bundler for development and production builds.

## Update JavaScript startup

Popcorn 0.3 returned a `Popcorn` instance or rejected its promise:

```typescript
const popcorn = await Popcorn.init({
  bundlePaths: ["/wasm/bundle.avm"],
});
```

Popcorn 0.4 returns a result object:

```typescript
import { Popcorn } from "@swmansion/popcorn";

const result = await Popcorn.init({
  onStdout: console.log,
  onStderr: console.error,
});

if (!result.ok) throw result.error;
const popcorn = result.data;
```

Remove these old initialization options:

- `bundlePaths`
- `container`
- `wasmDir`
- `onReload`
- `heartbeatTimeoutMs`
- `debug`

The bundler plugin now supplies the generated runtime asset locations.
Use `beam.otpAssetsRoot` only when you host OTP assets at a custom location.

Use separate boot phases when the application must receive startup events:

```typescript
const popcorn = new Popcorn();
const removeHandler = popcorn.onEvent((event) => console.log(event));

const result = await popcorn.boot();
if (!result.ok) throw result.error;
```

`deinit()` now stops the Web Worker.
You can call `boot()` on the same instance after shutdown.

## Replace custom calls with GenServer calls

Popcorn 0.3 sent calls to a custom process mailbox:

```typescript
const result = await popcorn.call(
  ["add", 2],
  { process: "counter", timeoutMs: 5_000 },
);
```

The receiving process used <code>Popcorn.Wasm.handle_message!/2</code>:

```elixir
def handle_info(message, state) when Popcorn.Wasm.is_wasm_message(message) do
  Popcorn.Wasm.handle_message!(message, fn
    {:wasm_call, ["add", amount]} ->
      count = state.count + amount
      {:resolve, count, %{state | count: count}}
  end)
end
```

Popcorn 0.4 sends a standard GenServer call through `Popcorn.Proxy`:

```typescript
const result = await popcorn.genserver.call("counter", ["add", 2], {
  timeoutMs: 5_000,
});

if (!result.ok) throw result.error;
console.log(result.data);
```

Handle the call with `handle_call/3`:

```elixir
@impl true
def handle_call(["add", amount], _from, state) do
  count = state.count + amount
  {:reply, count, %{state | count: count}}
end
```

The JavaScript target is now the first argument.
Popcorn does not use a default receiver.

The target can be a registered process name or a PID from the same runtime boot.
Calls use a five-second timeout by default.

The new result does not contain `durationMs`.
Measure elapsed time in application code when you need it.

## Replace custom casts with GenServer casts

Update a Popcorn 0.3 cast:

```typescript
popcorn.cast("reset", { process: "counter" });
```

Use the Popcorn 0.4 GenServer API:

```typescript
const result = await popcorn.genserver.cast("counter", "reset");
if (!result.ok) throw result.error;
```

Handle it with `handle_cast/2`:

```elixir
@impl true
def handle_cast("reset", _state) do
  {:noreply, %{count: 0}}
end
```

The new cast method is asynchronous.
A successful result confirms delivery to `Popcorn.Proxy`.
It does not confirm that the target handled the cast.

## Use direct process messages when you do not need GenServer

Popcorn 0.4 adds a direct send operation:

```typescript
const result = await popcorn.send("worker", { action: "refresh" });
if (!result.ok) throw result.error;
```

The process receives this message:

```elixir
{:wasm, %{"action" => "refresh"}}
```

Match the tuple directly or use `Popcorn.Wasm.is_message/1` in a guard.
This operation does not use `Popcorn.Proxy`.

## Update events sent to JavaScript

Popcorn 0.3 sent a separate event name and payload:

```elixir
Popcorn.Wasm.send_event("counter_updated", %{count: count})
```

The JavaScript handler received two arguments:

```typescript
popcorn.onMessage((eventName, payload) => {
  console.log(eventName, payload);
});
```

Popcorn 0.4 sends one value:

```elixir
Popcorn.Wasm.send(%{event: "counter_updated", count: count})
```

The JavaScript handler receives that value:

```typescript
const removeHandler = popcorn.onEvent((event) => {
  console.log(event.event, event.count);
});
```

Include an event name in the value when the application needs one.
Register the handler before `boot()` when the application sends startup events.

`onEvent()` returns a function that removes the handler.
Popcorn 0.4 removes `registerLogListener()` and `unregisterLogListener()`.
Pass `onStdout` and `onStderr` when you create the Popcorn instance.

## Update JavaScript calls from Elixir

Popcorn 0.3 passed one object to the JavaScript function:

```elixir
Popcorn.Wasm.run_js(
  """
  ({args}) => {
    document.querySelector(args.selector).textContent = args.text;
    return [args.text];
  }
  """,
  %{selector: "#status", text: "Ready"},
  return: :value
)
```

Popcorn 0.4 passes the arguments and bridge actions separately:

```elixir
Popcorn.Wasm.run_js(
  """
  ({selector, text}, {send, call, cast}) => {
    document.querySelector(selector).textContent = text;
    return text;
  }
  """,
  %{selector: "#status", text: "Ready"}
)
```

Return a normal value directly.
Remove the `:return` option and array wrapper.

The function no longer receives `wasm` or `iframeWindow`.
Use the `send`, `call`, and `cast` actions to contact BEAM processes.

The non-raising function returns `{:ok, value}` or `{:error, reason}`.
Use `run_js!/3` when a JavaScript failure must raise an exception.

The current bridge evaluates JavaScript source in the page.
The page Content Security Policy must permit `unsafe-eval`.

## Replace tracked objects

Popcorn 0.3 used `%Popcorn.TrackedObject{}` and separate value lookup functions.
Popcorn 0.4 uses an opaque `Popcorn.Wasm.tracked_value()` handle.

Create the handle in the JavaScript function:

```elixir
element =
  Popcorn.Wasm.run_js!(
    """
    () => new TrackedValue(document.querySelector("#status"))
    """
  )

Popcorn.Wasm.run_js!(
  "({element}) => element.replaceChildren()",
  %{element: element}
)
```

Add a cleanup function when the JavaScript value owns a resource:

```javascript
return new TrackedValue(controller, () => controller.abort());
```

Remove calls to these Popcorn 0.3 functions:

- <code>Popcorn.Wasm.get_tracked_values/1</code>
- <code>Popcorn.Wasm.get_tracked_values!/1</code>
- <code>Popcorn.Wasm.register_event_listener/2</code>
- <code>Popcorn.Wasm.unregister_event_listener/1</code>

Use `run_js/3` to add browser event listeners.
Use the supplied `send` action inside the listener.

## Review values across the bridge

Popcorn 0.3 accepted JSON-compatible values.
Popcorn 0.4 uses a typed ETF bridge with more BEAM value types.

Important JavaScript-to-BEAM conversions include:

| JavaScript value | BEAM value |
| --- | --- |
| String | UTF-8 binary |
| Safe integer | Integer |
| Other finite number | Float |
| Array | List |
| Plain object | Map with binary keys |
| `null` or `undefined` | `nil` |
| `atom("ok")` | Existing atom |
| `tuple(a, b)` | Tuple |

Import `atom` and `tuple` from `@swmansion/popcorn` when JavaScript must create these terms.
Popcorn rejects unknown atoms, cyclic objects, class instances, functions, and unsafe numbers.

BEAM PIDs become opaque JavaScript values.
Do not use a PID with another Popcorn instance or another runtime boot.

See [Values across the bridge](values.html) for the complete conversion rules.

## Review runtime and deployment limits

The OTP runtime supports standard processes, supervisors, monitors, timers, and message passing.
Do not assume that all native OTP features work in a browser.

Check these application requirements:

1. Remove native TCP, UDP, and Erlang distribution dependencies.
2. Remove operating-system subprocess calls.
3. Check each dependency for dynamic native implemented functions (NIFs).
4. Use the `crypto` runtime variant for `crypto`, `public_key`, and `ssl`.
5. Copy required `priv` files with the application build.
6. Add required runtime configuration to the browser application.

The plugin sets cross-origin isolation headers for Vite development and preview.
Your production server must set the same headers.

See [Compatibility and browser limits](compatibility.html) and
[Deploy Popcorn](deployment.html) before the production release.

## Complete the migration

Use this checklist after the application builds:

1. Confirm that `mix compile` succeeds with the selected Popcorn toolchain.
2. Confirm that the bundler plugin names the correct OTP application.
3. Confirm that no build script creates or copies an `.avm` file.
4. Confirm that `Popcorn.init()` handles both result variants.
5. Confirm that each GenServer call has a supervised `Popcorn.Proxy`.
6. Confirm that every JavaScript call or cast has an explicit target.
7. Confirm that browser events use one payload value.
8. Confirm that `run_js/3` functions use the new arguments.
9. Confirm that the production server uses HTTPS, COOP, and COEP.
10. Test one message in each bridge direction in the production build.

Use these migrated examples as working references:

- [`hello-popcorn`](https://github.com/software-mansion/popcorn/tree/main/examples/hello-popcorn) shows startup and browser JavaScript calls.
- [`game-of-life`](https://github.com/software-mansion/popcorn/tree/main/examples/game-of-life) shows supervision and a GenServer call.
- [`eval-in-wasm`](https://github.com/software-mansion/popcorn/tree/main/examples/eval-in-wasm) shows calls, timeouts, and terminal output.
- [`iex-wasm`](https://github.com/software-mansion/popcorn/tree/main/examples/iex-wasm) shows terminal input and output.
