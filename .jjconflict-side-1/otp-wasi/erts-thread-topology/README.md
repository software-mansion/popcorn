# WASI bounded ERTS thread-topology experiment

This isolated experiment tests whether OTP 28.3.1's minimum intended ERTS
thread shape fits Popcorn's hard budget of eight browser workers. It runs the
unchanged pthread-backed ethread sources through `ethr_thr_create`; it does not
claim that scheduler bodies or the emulator link on WASI yet.

## Source-derived topology

The intended minimal emulator arguments are `+S 1 +SDcpu 1 +SDio 1 +A 0`.
They imply these long-lived native child threads:

| Child role | Count | OTP 28.3.1 evidence |
| --- | ---: | --- |
| Normal scheduler | 1 | `erl_process.c`, `erts_start_schedulers()` |
| Dirty CPU scheduler | 1 | `erl_process.c`; `erts_init_scheduling()` asserts at least one |
| Dirty I/O scheduler | 1 | `erl_process.c`; minimum argument is one |
| Standard auxiliary thread | 1 | `erts_init_scheduling()` adds one and `erts_start_schedulers()` starts it |
| I/O poll thread | 1 | `erl_check_io.c` makes one the default/minimum |
| System-message dispatcher | 1 | `erl_trace.c`, unconditional `init_sys_msg_dispatcher()` |

That is six child workers plus the VM/main worker: seven workers for the
minimal topology. `+A 0` removes the optional async-driver pool. Run-queue
supervision defaults to disabled. A `clock_gettime` monotonic clock avoids the
`times()`-only monotonic extender. A WASI port must exclude Unix signal
dispatch, as the existing Emscripten port already does.

The fixed host prewarms seven child workers before starting the VM/main worker,
so the browser total is exactly eight. Six children represent the required
roles. A seventh capacity-spare child fills the final slot. While all seven are
live, an eighth child creation calls the same `ethr_thr_create` path and returns
`EAGAIN` (6) synchronously. No worker is allocated or reused.

## Smallest ERTS linkage boundary

`erl_threads.h` contains the smallest real ERTS wrapper around ethread, but
`erts_thr_create()` deliberately turns creation failure into a fatal VM error,
so it cannot express the capacity assertion. The first representative owner of
the minimal pool is `erl_process.c::erts_start_schedulers()`.

Compiling unchanged `erl_process.c` for `wasm32-wasip1-threads` stops before
that function at `emulator/sys/unix/erl_unix_sys.h`: wasi-libc requires explicit
signal and process-clock emulation, requires nonstandard Wasm SJLJ support for
`setjmp`, and does not provide `pwd.h`. Linking `erl_process.c` would also pull
in emulator run queues, allocators, polling, tracing, and process state. This is
the first precise broader-port boundary; the experiment does not copy or
reimplement those ERTS abstractions to manufacture a passing result.

## Runtime probe

Each of the seven child functions:

- starts concurrently through OTP's actual `ethr_thr_create` implementation;
- records a distinct `ethr_self()` identity and private ethread TSD value;
- signals readiness, then remains blocked behind a condition variable;
- resumes together, verifies its TSD again, returns a role-specific result,
  and is joined cleanly.

The test boundary consists only of structured progress and result imports.
The host reports total prewarmed, peak, active, and reused worker counts. Every
child worker asserts that it receives exactly one start request.

## Build and verification

Requirements are wasi-sdk 33, `wasm-tools`, pnpm, and Chromium:

```sh
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm build
pnpm lint
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm test
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm exec playwright test --repeat-each=5 --workers=1
```

The build validates the module with `wasm-tools validate`; the Playwright test
asserts the exact ten imports and two exports recorded in
`module-inspection.txt`.

Measured Chromium result:

- required ERTS-shaped children: 6;
- live/joined children including capacity spare: 7;
- capacity failure: `EAGAIN` (6), synchronous;
- distinct identities and TSD: true;
- total prewarmed workers: 8;
- peak workers: 8;
- active workers after teardown: 0;
- reused workers: 0;
- focused run and five serial repeat runs: passed.

## Conclusion

**Conditional go.** The minimum source-derived long-lived topology fits within
eight workers, and the underlying OTP ethread lifecycle is deterministic at
capacity without reuse. The next unresolved boundary is a real WASI ERTS
system layer/configuration that gets `erl_process.c` through compilation and
linking. Until that port exists, this result does not prove that the scheduler,
poll, auxiliary, or system-message thread bodies execute in Chromium.

Compatibility shims remain identical to `ethread-primitives`: fixed CPU-count
queries, `pthread_attr_setscope` returning `ENOTSUP`, and unused pipe fallback
stubs removed by LTO/section GC. Unsupported broader APIs at the ERTS boundary
are Unix signals, process-clock emulation, SJLJ/setjmp, and `pwd.h` account
lookups. Generated Wasm, WAT, bundles, reports, and toolchains remain ignored.
