# Migrate from Popcorn 0.3

Popcorn 0.4 replaces AtomVM with OTP/BEAM compiled to WebAssembly and changes
the JavaScript API. Use this guide to update your project.

## Build and package changes

Popcorn no longer creates an `.avm` bundle with `mix popcorn.cook`. The bundler
plugin now packages compiled BEAM files and application dependencies.

Run `mix compile` before the JavaScript build. Configure `rootDir` and `app` in
the Vite, Rollup, or esbuild plugin.

The new Elixir package does not include these Mix tasks:

- `mix popcorn.cook`
- `mix popcorn.gen.js`
- `mix popcorn.build_runtime`
- `mix popcorn.server`

## JavaScript API changes

`Popcorn.init()` now returns a result object:

```typescript
const result = await Popcorn.init();
if (!result.ok) throw result.error;

const popcorn = result.data;
```

Use explicit process targets:

| Popcorn 0.3                        | Popcorn 0.4                               |
| ---------------------------------- | ----------------------------------------- |
| `popcorn.call(payload, {process})` | `popcorn.genserver.call(target, payload)` |
| `popcorn.cast(payload, {process})` | `popcorn.genserver.cast(target, payload)` |
| `popcorn.onMessage(handler)`       | `popcorn.onEvent(handler)`                |
| `registerLogListener`              | `onStdout` and `onStderr` options         |
| `bundlePaths`                      | Bundler plugin application packaging      |

Calls default to a five-second timeout. A timeout does not cancel server work.

## Elixir API changes

Use normal OTP application startup. Remove the old readiness callback and
default receiver setup.

Add `Popcorn.Proxy` to the supervision tree for JavaScript calls and casts.
Handle them with normal `handle_call/3` and `handle_cast/2` callbacks.

Use `Popcorn.Wasm.send/1` instead of `send_event/2`. Use `run_js/3` or
`run_js!/3` for browser calls.

The new `run_js` function signature is `(args, {send, call, cast}) => result`.
Return the result directly.

## Review runtime assumptions

OTP provides much more standard library behavior than AtomVM. The browser
sandbox still removes native sockets, subprocesses, distribution, and dynamic
NIF loading.

Review `priv` files and runtime configuration because the packager does not
copy them automatically.
