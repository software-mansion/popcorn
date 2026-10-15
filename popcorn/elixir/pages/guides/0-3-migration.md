# Migrating from Popcorn 0.3

Popcorn 1.0 replaces AtomVM with Erlang/OTP compiled to WebAssembly.
This change removes the AtomVM bundle and changes APIs.

Use this guide to migrate an application from Popcorn 0.3.
The guide uses Vite, but the Rollup and esbuild plugins use the same options.

## Review the main changes

| Popcorn 0.3                                                    | Popcorn 1.0                                                                                                                                                   |
| -------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| AtomVM runs in a hidden iframe.                                | The Erlang/OTP BEAM runs in a Web Worker.                                                                                                                     |
| `mix popcorn.cook` creates one `.avm` bundle.                  | `mix popcorn.cook` packages applications into a tarball.                                                                                                      |
| The application calls `Popcorn.Wasm.ready/1`.                  | OTP starts the application through its normal application callback. You can use `Popcorn.Wasm.await_ready/1` to wait for JavaScript initialization to finish. |
| `cast()` and `calls()` are plain messages, processed manually. | JavaScript can use normal GenServer calls and casts.                                                                                                          |
| JavaScript methods can throw on errors.                        | Bridge operations throw; `try*` variants return typed result objects.                                                                               |

The new runtime supports more OTP behavior than AtomVM.
It still operates inside the browser sandbox.

## Update the packages and toolchain

Replace the Popcorn dependency in `mix.exs`:

```elixir
defp deps do
  [
    {:popcorn, "1.0.0-next.0"}
  ]
end
```

Install the matching JavaScript prerelease if your project uses JavaScript bundler:

```bash
npm install @swmansion/popcorn@next
```

The Elixir and JavaScript package versions must match.
Popcorn 1.0 requires Elixir 1.19 or later.

Use the toolchain from the selected Popcorn release, the packager checks the host OTP version against the WebAssembly runtime.

Then get the dependencies and compile the application:

```bash
mix deps.get
mix compile
```

## Use normal OTP application startup

Remove these calls from application startup:

- `Popcorn.Wasm.ready/0`
- `Popcorn.Wasm.ready/1`
- `Popcorn.Wasm.set_default_receiver/1`

Popcorn 1.0 starts the application from its `.app` specification.
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
      MyApp.Worker
    ]

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: MyApp.Supervisor
    )
  end
end
```

## Replace `.avm` bundle creation

Delete the Popcorn 0.3 output configuration:

```elixir
config :popcorn, out_dir: "dist/wasm"
```

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
It packages the application, its dependencies, and the boot file.

Remove the old `treeshake` and `extra_beams` build options. See `mix popcorn.cook` to check current treeshake options. Treeshaking is disabled by default.
The new `strip` option removes nonessential BEAM chunks but does not remove functions.

Use `extraApps` for applications that dependency analysis cannot find:

```typescript
popcorn({
  rootDir: "../",
  extraApps: ["eex"],
});
```

Use `app: null` when the runtime must not start an application.

## Update JavaScript startup

Popcorn 0.3 returned a `Popcorn` instance or rejected its promise:

```typescript
const popcorn = await Popcorn.init({
  bundlePaths: ["/wasm/bundle.avm"],
});
```

Popcorn 1.0 returns an instance or rejects:

```typescript
import { Popcorn } from "@swmansion/popcorn";

const popcorn = await Popcorn.init({
  onStdout: console.log,
  onStderr: console.error,
});
```

Initialization rejects on failure. Only `send`, `call`, and `cast` have `try` variants that return result objects.

Remove these old initialization options:

- `bundlePaths`
- `container`
- `wasmDir`
- `onReload`
- `heartbeatTimeoutMs`
- `debug`

The bundler plugin now supplies the generated runtime asset locations.
Use `beam.otpAssetsRoot` only when you host OTP assets at a custom location.

`deinit()` now stops the Web Worker.
You can call `boot()` on the same instance after shutdown.

## Replace custom calls with GenServer calls

Popcorn 0.3 sent calls to a custom process mailbox:

```typescript
const result = await popcorn.call(["add", 2], {
  process: "counter",
  timeoutMs: 5_000,
});
```

The receiving process used `Popcorn.Wasm.handle_message!/2`:

```elixir
def handle_info(message, state) when Popcorn.Wasm.is_wasm_message(message) do
  Popcorn.Wasm.handle_message!(message, fn
    {:wasm_call, ["add", amount]} ->
      count = state.count + amount
      {:resolve, count, %{state | count: count}}
  end)
end
```

Popcorn 1.0 sends a standard GenServer call through `Popcorn.Proxy`:

```typescript
const result = await popcorn.call("counter", ["add", 2], {
  timeoutMs: 5_000,
});

console.log(result);
```

Handle the call with `handle_call/3`:

```elixir
@impl true
def handle_call(["add", amount], _from, state) do
  count = state.count + amount
  {:reply, count, %{state | count: count}}
end
```

- The JavaScript target is now the first argument.
- The target can be a registered process name or a PID from the same runtime boot.
- Calls use a five-second timeout by default.
- The reply does not contain `durationMs`.
- Measure elapsed time in application code when you need it.

## Replace custom casts with GenServer casts

Update a Popcorn 0.3 cast:

```typescript
popcorn.cast("reset", { process: "counter" });
```

Use the Popcorn 1.0 GenServer API:

```typescript
await popcorn.cast("counter", "reset");
```

Handle it with `handle_cast/2`:

```elixir
@impl true
def handle_cast("reset", _state) do
  {:noreply, %{count: 0}}
end
```

- The new cast method is asynchronous.
- A successful cast confirms delivery to `Popcorn.Proxy`.
  It does not confirm that the target handled the cast.

## Use direct process messages when you do not need GenServer

Popcorn 1.0 adds a direct send operation:

```typescript
await popcorn.send("worker", { action: "refresh" });
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

Popcorn 1.0 sends one value:

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

`onEvent()` returns a function that removes the handler.
Popcorn 1.0 removes `registerLogListener()` and `unregisterLogListener()`.

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

Popcorn 1.0 passes the arguments and bridge actions separately:

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

- Return a normal value directly.
- Remove the `:return` option and array wrapper.
- The function no longer receives `wasm` or `iframeWindow`.
  Use the `send`, `call`, and `cast` actions to contact BEAM processes.
- The non-raising function returns `{:ok, value}` or `{:error, reason}`.
  Use `run_js!/3` when a JavaScript failure must raise an exception.
- The current bridge evaluates JavaScript source in the page.
  The page Content Security Policy must permit `unsafe-eval`.

## Replace tracked objects

Popcorn 0.3 used `%Popcorn.TrackedObject{}` and separate value lookup functions.
Popcorn 1.0 uses an opaque `Popcorn.Wasm.tracked_value()` handle.

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

- `Popcorn.Wasm.get_tracked_values/1`
- `Popcorn.Wasm.get_tracked_values!/1`
- `Popcorn.Wasm.register_event_listener/2`
- `Popcorn.Wasm.unregister_event_listener/1`

Use `run_js/3` to add browser event listeners.
Use the supplied `send` action inside the listener.

## Review values across the bridge

Popcorn 0.3 accepted JSON-compatible values.
Popcorn 1.0 uses a typed ETF bridge with more BEAM value types.

Important JavaScript-to-BEAM conversions include:

| JavaScript value      | BEAM value           |
| --------------------- | -------------------- |
| String                | UTF-8 binary         |
| Safe integer          | Integer              |
| Other finite number   | Float                |
| Array                 | List                 |
| Plain object          | Map with binary keys |
| `null` or `undefined` | `nil`                |
| `atom("ok")`          | Existing atom        |
| `tuple(a, b)`         | Tuple                |

Import `atom` and `tuple` from `@swmansion/popcorn` when JavaScript must create these terms.
Popcorn rejects unknown atoms, cyclic objects, class instances, functions, and unsafe numbers.

BEAM PIDs become opaque JavaScript values.
Do not use a PID with another Popcorn instance or another runtime boot.

See [Values across the bridge](values.md) for the complete conversion rules.

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

See [Compatibility and browser limits](compatibility.md) and
[Deploy Popcorn](deployment.md) before the production release.

## Examples

Use these migrated examples as working references:

- [`hello-popcorn`](https://github.com/software-mansion/popcorn/tree/main/examples/hello-popcorn) shows startup and browser JavaScript calls.
- [`game-of-life`](https://github.com/software-mansion/popcorn/tree/main/examples/game-of-life) shows supervision and a GenServer call.
- [`eval-in-wasm`](https://github.com/software-mansion/popcorn/tree/main/examples/eval-in-wasm) shows calls, timeouts, and terminal output.
- [`iex-wasm`](https://github.com/software-mansion/popcorn/tree/main/examples/iex-wasm) shows terminal input and output.
