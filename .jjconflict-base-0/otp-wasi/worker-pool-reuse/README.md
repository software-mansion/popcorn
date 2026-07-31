# WASI worker-pool reuse experiment

This experiment tests whether Popcorn's eight prewarmed workers can safely
serve a larger number of sequential `wasm32-wasip1-threads` pthread lifetimes.

The probe creates three contending pthreads, validates mutexes, condition
variables, thread-local storage, and joined return values, then creates and
joins 100 more threads sequentially. The program reports its result through an
imported `experiment.report` callback; the browser test asserts that object,
not console output.

## Requirements

- Chromium with WebAssembly threads support.
- COOP `same-origin` and COEP `require-corp`. Vite applies both headers for
  development and preview.
- [wasi-sdk 33](https://github.com/WebAssembly/wasi-sdk/releases/tag/wasi-sdk-33)
  with its `wasm32-wasip1-threads` sysroot.

## Build and test

```sh
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm build
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm test
```

Use the SDK directory appropriate for the host architecture. `build-wasm.sh`
uses the following compiler invocation:

```sh
clang --target=wasm32-wasip1-threads --sysroot="$WASI_SDK_PATH/share/wasi-sysroot" \
  -O2 -pthread -matomics -mbulk-memory -Wl,--import-memory \
  -Wl,--shared-memory -Wl,--max-memory=16777216 \
  -Wl,--export=wasi_thread_start -Wl,--allow-undefined -o public/program.wasm src/main.c
```

`module-inspection.txt` records the expected import and export surface. The
build also runs `wasm-tools validate` and writes an ignored text-format module
for local inspection.

## Host boundary

This experiment deliberately uses a small project-owned host rather than an
Emscripten runtime or a general WASI shim. The module needs only `proc_exit`,
clock, descriptor no-ops, scheduling yield, and `wasi.thread-spawn`; keeping
those imports explicit exposes the ABI and worker lifecycle under test. The
thread-spawn implementation follows the [WASI threads proposal](https://github.com/WebAssembly/wasi-threads): it creates a new module instance in an
available worker, imports the same shared memory, assigns a non-duplicate live
ID, and invokes `wasi_thread_start(tid, start_arg)`. Eight workers are
prewarmed before the VM starts. A worker marks its shared pool slot idle only
after its thread-start trampoline returns, allowing later sequential threads to
reuse it without growing the pool.

The C pthread library remains wasi-libc's implementation. This host does not
add a pthread library or patch wasi-libc.

## Result

The eight-slot host uses a shared atomic availability record and releases a
slot after `wasi_thread_start` returns. Chromium still hangs while reusing a
slot under the synchronous `pthread_create` ABI. This is a no-go for this
direct reuse protocol; it does not establish that a bounded ERTS pool is
impossible.
