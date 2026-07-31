# WASI OTP ethread primitives experiment

This experiment compiles OTP 28.3.1's actual pthread-backed `ethread` library for
`wasm32-wasip1-threads` and runs a representative primitive probe in Chromium.
It is the next boundary after `threads-browser`: the C harness calls OTP's
`ethr_*` API rather than pthreads directly.

The probe verifies `ethr_init`, `ethr_late_init`, three `ethr_thr_create` and
`ethr_thr_join` lifecycles, thread identities, thread-specific data, mutexes,
condition variables, an `ethr_event`, and 32-bit atomics. Two children contend
behind a release condition; a third blocks on the event. The browser test
asserts the structured result and complete worker teardown.

## Requirements

- Chromium with WebAssembly threads support.
- COOP `same-origin` and COEP `require-corp`.
- wasi-sdk 33 with its `wasm32-wasip1-threads` sysroot.
- An OTP 28.3.1 source checkout. The default is this repository's ignored
  `otp/sources/otp`; set `OTP_SOURCE_PATH` to use another checkout.

## Build and test

```sh
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm build
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm test
```

The build compiles these unchanged OTP sources:

- `erts/lib_src/common/ethr_aux.c`
- `erts/lib_src/common/ethr_atomics.c`
- `erts/lib_src/common/ethr_mutex.c`
- `erts/lib_src/common/ethr_cbf.c`
- `erts/lib_src/pthread/ethread.c`
- `erts/lib_src/pthread/ethr_event.c`

`config/ethread_header_config.h` records the wasm32/pthread/atomic feature
values that a future OTP WASI configure target must generate. The experiment
supplies only two narrow platform seams outside OTP:

- `otp_cpu_info.c` supplies the CPU-count queries used during ethread
  initialization, without pulling the emulator's unrelated topology parser
  into this probe.
- `wasi_compat.c` returns `ENOTSUP` for `pthread_attr_setscope`, which ethread
  explicitly accepts, and fails the unused pipe fallback with `ENOSYS`.

wasi-libc hides declarations for unsupported signal and pipe APIs even though
OTP's pthread implementation defines optional entry points that mention them.
`wasi_compat.h` makes those declarations visible for compilation. Link-time
optimization and section garbage collection remove the unused signal and
pipe/select paths; the inspected final module has no such imports.

## Browser host

The host prewarms three one-shot child workers before starting the VM worker.
Each `wasi.thread-spawn` call receives a distinct worker and shared memory.
This intentionally does not reuse workers: `worker-pool-reuse` already records
why that direct synchronous reuse protocol hangs, and repeating it here would
confound the ethread result.

Progress callbacks make a timeout identify the last completed VM or child
phase. The final result callback reports initialization, mutex/condition,
event, atomic, child-completion, and worker-lifecycle results.

## Result

Chromium completes the probe with three prewarmed child workers and zero active
workers after teardown. Five serial repeat runs pass. This is a **go** for the
next ERTS/WASI experiment: OTP's core pthread-backed ethread primitives work on
wasi-sdk 33 without changing OTP sources.

This is not a whole-ERTS result. A real port still needs an OTP configure target
and WASI implementations or exclusions for CPU topology, signals, timed-event
pipe/select fallback, filesystem, polling, timers, and sockets. It also still
needs a host design that resolves the separate bounded-worker reuse problem.
