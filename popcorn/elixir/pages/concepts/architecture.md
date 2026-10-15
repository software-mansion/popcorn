# Architecture

## Overview

Popcorn works by:

- Compiling OTP/BEAM to Wasm, using Emscripten. The VM itself is patched to either adjust the VM code to work in a browser context or remove features unsupported there.
- Running the VM in multiple Web Worker threads which communicate with the JavaScript API using `postMessage()`. This gives us isolation: a Web Worker crash doesn't affect the main page.
- Providing some API surface to hook the VM to JavaScript. For example, sending messages to processes works through the `:wasm` module which exposes a JavaScript API to Emscripten, and at the same time, exposes an Erlang API for Popcorn modules to use. The `:wasm` module is a wrapper for a NIF (which is written in C).
- Including the batteries: packaging applications into tarballs, reducing their size, exposing adapters for low-level APIs (Req adapter, terminal API), and making sure that this C-JS-Erlang-Elixir-Wasm amalgamation works well.

<div style="text-align: center;">
<img src="assets/architecture-light.svg#gh-light-mode-only" style="width: 700px; max-width: 100%;" alt="Popcorn runtime: browser API, VM worker, and BEAM with OTP applications on Emscripten pthread workers." />
<img src="assets/architecture-dark.svg#gh-dark-mode-only" style="width: 700px; max-width: 100%;" alt="Popcorn runtime: browser API, VM worker, and BEAM with OTP applications on Emscripten pthread workers." />
</div>

## Packaging

While Popcorn keeps the BEAM bytecode format, it still post-processes `.beam` files before they are executed in the browser. Most notably, it strips unused chunks and can tree-shake parts of the code that are unreachable.
We also pack the apps into tarballs to ship a few bigger files instead of a lot of smaller ones. This allows us to introspect the files themselves and build a metadata file about them. This is important for selecting the correct VM variant (Popcorn ships crypto-less and crypto-full VM versions) and detecting incompatibilities.

Popcorn provides the `mix popcorn.cook` task that does that and Vite/esbuild/Rollup plugins that do the same.

## Initialization

Booting the VM works roughly as follows:

- Request all needed assets (`.wasm`, tarballs, Popcorn JavaScript files).
- Register handlers for events coming from the VM in the Popcorn class instance. Start the timers which will fire if the booting process takes too long.
- Create the main Web Worker which loads the Emscripten module of the VM. This creates new Web Workers (e.g. for schedulers) in turn.
- Run Erlang's boot file, which loads core modules and starts the typical OTP `init` process. This starts applications and their supervision trees.
- After all apps (including the user's) are up, finish the initialization from the JavaScript side, resolve the `Popcorn.init()` promise and give control back to the user.

JavaScript and the VM aren't synchronized in execution and you need explicit messages to wait on each other.

## Sending messages

The `:wasm` module mentioned before is mostly responsible for serialization of messages between JavaScript and Elixir. If the messages are sent from `cast()` or `call()`, we route them through `Popcorn.Proxy`, which forwards the messages to their destination and handles timeouts and non-existent target processes. By default, Popcorn includes only one `Proxy` in its supervision tree but it's possible to create more.
All messages (GenServer or not) still always pass through a single `:wasm` module.

## Handles

Popcorn introduces `TrackedValue` handles for JavaScript values, allowing them to be passed to Elixir. JavaScript keeps the values in a map, and entries in it are keyed by handles (they're treated specially during serialization). We also allow attaching a cleanup function to the `TrackedValue` which is run after the VM loses the handle (garbage collected). The cleanup runs asynchronously.
We also have `Pid` values which are terms accessible from JavaScript. They're used for specifying the target process in `send()`, `call()`, and `cast()`. See ["Sharing values across languages"](values.md) for the other supported value types.
