# WASI/ERTS scheduler-startup boundary

This independent proof of concept crosses the previous
`erl_unix_sys.h` compile boundary for OTP 28.3.1 on
`wasm32-wasip1-threads`. The unchanged `erl_process.c` now compiles to a
validated WebAssembly object containing the real `erts_init_scheduling()` and
`erts_start_schedulers()` implementations.

It does not link or run those functions. Linking even the initialization-only
harness reaches a precise 19-symbol boundary across core emulator subsystems.
Adding those systems would be a substantially broader emulator port, so this
POC stops there instead of stubbing ERTS behavior or reporting a modeled
scheduler result.

## Configuration and compatibility

The compile uses OTP's generated Emscripten `config.h` as the closest existing
32-bit threaded configuration, then overrides mmap/madvise detection locally.
The WASI target is configured explicitly with:

- wasi-libc signal emulation;
- wasi-libc process-clock emulation;
- wasi-libc mmap emulation for headers used elsewhere in ERTS;
- LLVM Wasm SJLJ;
- local declarations for unavailable account, resolver, timezone, pthread
  signal, and pipe APIs.

`config/pwd.h` declares only the `passwd` fields and `getpwuid()` signature
referenced by the Unix system layer. `config/netdb.h` forward-declares resolver
types because WASI has no socket resolver header. `src/wasi_compat.c` supplies
the same accepted `pthread_attr_setscope(ENOTSUP)` and unused `pipe(ENOSYS)`
seams as the earlier ethread probe, plus a no-op `tzset()` for WASI's fixed host
environment. `src/otp_cpu_info.c` supplies the fixed CPU-count query needed by
ethread initialization.

No OTP source, wasi-libc source, or pthread implementation is patched.

## Commands

From this directory:

```sh
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos \
  bash scripts/compile-erl-process.sh

WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos \
  bash scripts/link-erts-init.sh

WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos \
  bash scripts/verify-boundary.sh
```

The first command compiles and validates `public/erl_process.o`, and asserts
that both scheduling entry points are defined. The second command is expected
to fail. The third command proves that its failure is exactly the recorded
19-symbol frontier.

## Exact source boundary

The compile artifact uses unchanged OTP 28.3.1
`erts/emulator/beam/erl_process.c`. The link probe additionally compiles these
unchanged sources whose WASI behavior was already proven by
`ethread-primitives`:

- `erts/lib_src/common/ethr_aux.c`
- `erts/lib_src/common/ethr_atomics.c`
- `erts/lib_src/common/ethr_mutex.c`
- `erts/lib_src/common/ethr_cbf.c`
- `erts/lib_src/pthread/ethread.c`
- `erts/lib_src/pthread/ethr_event.c`

Section garbage collection reduces the unresolved initialization path to:

- allocator state and permanent/cache-aligned allocation:
  `erl_alloc.c`, `erl_alloc_util.c`;
- scheduler-specific preallocation: `erl_sched_spec_pre_alloc.c`;
- process locking: `erl_process_lock.c`;
- thread progress and queues: `erl_thr_progress.c`, `erl_thr_queue.c`;
- port tasks and I/O wakeup: `erl_port_task.c`, `erl_check_io.c`;
- atom-cache and scheduler-unique state: `external.c`, `erl_bif_unique.c`;
- Unix monotonic time: `sys/unix/sys_time.c`;
- VM constants, fatal exit, and dirty allocator configuration owned by
  `erl_init.c`, `bif.c`, and allocator initialization.

The exact 19 symbols are recorded in `module-inspection.txt`. They are not safe
compatibility stubs: `erts_init_scheduling()` allocates and initializes real
run queues, scheduler data, sleep state, process tables, locks, progress data,
and port-task state through them. Supplying dummy definitions would manufacture
a passing startup while violating ERTS invariants.

## Verification result

- TypeScript checking: not applicable; no browser host is produced before the
  link boundary.
- wasi-sdk 33 `wasm32-wasip1-threads` compilation: passed.
- `wasm-tools validate public/erl_process.o`: passed.
- exact scheduling-symbol assertions: passed.
- exact final-module import/export assertions: not applicable; no final module
  links.
- focused Chromium test and five serial repeats: not run; Chromium cannot load
  a relocatable object.
- worker peak, teardown count, reuse count, capacity failure, thread identity,
  TSD, and lifecycle: not measured by this POC because no ERTS thread starts.

The earlier independent topology experiment remains the measured budget
evidence: eight prewarmed workers, peak eight, zero active after teardown, zero
reuse, and deterministic `EAGAIN` at capacity. Those numbers are not presented
as results of this real ERTS compile/link POC.

## Result

**Conditional go.** The initial WASI system-layer boundary is crossed and both
actual scheduling entry points compile unchanged for `wasm32-wasip1-threads`.
The next boundary is bounded and source-specific, but no real ERTS
initialization function has executed yet.

The next work is a deliberate core-emulator port of the allocator, process
locking, thread-progress/queue, port-task/I/O, atom-cache, scheduler-unique,
time, and fatal-exit initialization chain. Only after those real subsystems
link and their required initialization order is reproduced from `erl_init.c`
can this POC safely call `erts_init_scheduling()` and proceed toward
`erts_start_schedulers()` within the fixed eight-worker host.
