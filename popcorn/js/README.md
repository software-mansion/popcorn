# Popcorn JavaScript

JavaScript API and bundler plugins for running Elixir and Erlang applications
in the browser with Popcorn's WebAssembly runtime.

## Install

```sh
npm install @swmansion/popcorn@next
```

Use the matching Elixir package version. See the
[Elixir package README](https://github.com/software-mansion/popcorn/blob/main/popcorn/elixir/README.md)
for application setup, toolchain requirements, and packaging configuration.

## Bundler setup

For Vite:

```ts
import { defineConfig } from "vite";
import { popcorn } from "@swmansion/popcorn/vite";

export default defineConfig({
  plugins: [popcorn({ rootDir: "../" })],
});
```

Set `rootDir` to the Mix project directory. The plugin compiles and packages
the application and runtime assets.

Rollup and esbuild plugins use the same option, imported from
`@swmansion/popcorn/rollup` and `@swmansion/popcorn/esbuild`. Use ESM output.

## JavaScript API

```ts
import { Popcorn } from "@swmansion/popcorn";

const vm = await Popcorn.init();

vm.onEvent((event) => console.log(event));
const count = await vm.call("counter", ["add", 1]);
console.log(count);

// Stop the runtime when the application no longer needs it.
vm.deinit();
```

`Popcorn.init()` waits for the application's supervision tree to start.
This example assumes the application starts a GenServer registered as `:counter`
that handles `["add", amount]` requests.

Use `send()` for process messages, `call()` and `cast()` for GenServer requests,
and `onEvent()` for events from Elixir. Methods throw or reject on failure;
`trySend()`, `tryCall()`, and `tryCast()` return result objects instead.

See the [JavaScript API reference](https://hexdocs.pm/popcorn/JSLib.Popcorn.html)
for options, terminal IO, and memory limits. Follow the
[deployment guide](https://hexdocs.pm/popcorn/deployment.html)
for required response headers and Content Security Policy settings.
