# Architecture

<style>
  .popcorn-architecture { color-scheme: light; }
  body.dark .popcorn-architecture { color-scheme: dark; }
</style>

## Overview

Popcorn works by:

- Compiling OTP/BEAM to Wasm, using Emscripten. The VM itself is patched to either adjust the VM code to work in browsers context or remove features unsupported there.
- Running the VM in multiple Web Worker threads which communicate with JavaScript API using `postMessage()`. This gives us isolation, Web Worker crash doesn't affect main page.
- Providing some API surface to hook the VM to the JavaScript. For example, sending messages to processes works by `:wasm` module which exposes JavaScript API to Emscripten, and at the same time, exposes Erlang API for Popcorn modules to use. The `:wasm` module is a wrapper for a NIF (which is written in C).
- Including the batteries: packaging applications into tarballs, reducing their size, exposing adapters for low-level APIs (Req adapter, terminal API), and making sure that this C-JS-Erlang-Elixir-Wasm amalgamation works well.

<img class="popcorn-architecture" src="assets/architecture.svg" style="display: block; margin: auto; width: 700px; max-width: 100%;" alt="Popcorn runtime: browser API, VM worker, and BEAM with OTP applications on Emscripten pthread workers." />

## Packaging

While Popcorn doesn't modify `.beam` files executed in the browser, we still do post-processing on them. Most notably, stripping unused chunks and tree-shaking parts of the code that are unreachable.
We also pack the apps into tarballs to ship few bigger files instead of a lot of smaller ones. This allows us to introspect the files themselves and build a metadata file about them. This is important for selecting correct VM variants (Popcorn ships crypto-less and crypto-full VM versions) and detecting incompatibilities.

Popcorn provides `popcorn.cook` Mix task that does that and Vite/Esbuild/Rollup plugins that do the same.

## Initialization

Booting the VM works roughly as follows:

- Request all needed assets (`.wasm`, tarballs, Popcorn JavaScript files).
- Register handlers for events coming from the VM in the Popcorn class instance. Start the timers which will fire if booting process take too long.
- Create the main Web Worker which loads Emscripten module of the VM. This creates new Web Workers (e.g. for schedulers) in turn.
- Run Erlang's boot file, which loads core modules and starts typical OTP `init` process. This starts applications and their supervision trees.
- After all apps (including user's) are up, finish the initialization from JavaScript side, resolve the `Popcorn.init()` promise and give control back to the user.

JavaScript and VM aren't synchronized in execution and you need explicit messages to wait on each other.

## Sending messages

`:wasm` module mentioned before is mostly responsible for serialization of messages between JavaScript and Elixir. If the messages are sent from `cast()` or `call()`, we route them through `Popcorn.Proxy`, which forwards the messages to their destination and handle timeouts and non-existent target processes. By default, Popcorn includes only one `Proxy` in its supervision tree but it's possible to create more.
All messages (GenServer or not) still always pass through single `:wasm` module.

## Handles

Popcorn introduces `TrackedValues` which are handles for JavaScript values, allowing to pass them on Elixir side. Conceptually, this is just a weak-map on JavaScript side and handles are keys to it (and they're treated specially in serialization). We also allow attaching the cleanup function to the `TrackedValue` which is run after VM loses the handle (it's garbage collected). The cleanup runs asynchronously.
We also have `PID` values which are terms accessible from JavaScript. They're used for specifying target process in `send()`, `call()`, and `cast()`. Currently, we don't allow other term types.
