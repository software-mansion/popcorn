# WASI/ERTS scheduler-startup boundary

This independent proof of concept crosses the previous
`erl_unix_sys.h` compile boundary for OTP 28.3.1 on
`wasm32-wasip1-threads`. The unchanged `erl_process.c` now compiles to a
validated WebAssembly object containing the real `erts_init_scheduling()` and
`erts_start_schedulers()` implementations.

The safe-initialization harness reproduces the required allocator,
thread-progress, process-table, polling, time, monitor, and process-signal setup
before the scheduling call. This phase crosses the previous 20-symbol
process-lifecycle boundary with unchanged OTP sources for messages, copying,
bits, garbage collection, dictionaries, hashing, tracing, microstate
accounting, NIF scheduling, literal release, code indexes, node tables, and the
Unix dynamic-loader seam.

The link now stops at a precise four-symbol port/registry boundary. It does not
run `erts_init_scheduling()`: the remaining definitions are owned by `io.c` and
`register.c`, and retaining `io.c` begins the complete ERTS port/driver runtime.
That is a substantially broader emulator port, so this POC preserves the exact
boundary instead of stubbing port state.

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
environment. It also supplies WASI implementations of ERTS's Unix system
allocator, aligned allocation, fatal-exit, assertion, OS-version, and OS-flavor
seams. `src/otp_cpu_info.c` supplies the fixed CPU-count query needed by ethread
initialization.

`HAVE_DLOPEN` is disabled locally because WASI has no dynamic loader. The
unchanged `erl_unix_sys_ddll.c` then uses OTP's own unsupported-platform branch
and returns `ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY`; this is an existing OS feature
boundary, not a NIF behavior stub.

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
four-symbol port/registry frontier.

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

The safe harness additionally compiles unchanged OTP sources for:

- allocator state, strategies, and permanent/cache-aligned allocation;
- scheduler-specific preallocation: `erl_sched_spec_pre_alloc.c`;
- process tables and locking;
- thread progress and queues: `erl_thr_progress.c`, `erl_thr_queue.c`;
- port tasks, check-I/O, and polling;
- atom, index, hashing, Unicode, big integers, NIF support, and global literals;
- atom-cache and scheduler-unique state;
- high-level timers, time support, and Unix monotonic time;
- monitor/link and process-signal initialization;
- message allocation, queues, and off-heap cleanup: `erl_message.c`;
- copying and bit operations: `copy.c`, `erl_bits.c`;
- garbage collection: `erl_gc.c`;
- process dictionaries and term hashing: `erl_process_dict.c`,
  `erl_term_hashing.c`;
- NIF scheduling and microstate accounting: `erl_nfunc_sched.c`, `erl_msacc.c`;
- tracing and sessions: `erl_trace.c`, `beam_ranges.c`, `erl_bif_trace.c`;
- literal release and code indexes: `beam_bif_load.c`, `code_ix.c`;
- node/distribution cleanup: `erl_node_tables.c`;
- unsupported Unix dynamic loading: `erl_unix_sys_ddll.c`;
- the unchanged pthread ethread implementation.

The exact source list is the link command in `scripts/link-erts-init.sh`. Section
garbage collection reduces the remaining closure to `erts_port`,
`erts_port_free`, `erts_save_suspend_process_on_port`, and
`erts_whereis_name_to_id`. The first three are owned by `io.c`; the last is
owned by `register.c`. `code_ix.c` retains process suspension on a busy port,
while node deletion retains high-level timer/port-task cleanup and name lookup.
Supplying dummy definitions would manufacture a passing startup while violating
port-table and process-suspension invariants.

## Verification result

- TypeScript checking: not applicable; no browser host is produced before the
  link boundary.
- wasi-sdk 33 `wasm32-wasip1-threads` compilation: passed.
- `wasm-tools validate public/erl_process.o`: passed.
- exact scheduling-symbol assertions: passed.
- exact four-symbol expected-link-failure assertion: passed.
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

**Conditional go.** The full previous process-lifecycle frontier is crossed
with real OTP sources. The deepest source-derived harness remains
`erts_init_process()` followed by the linked-but-not-executed scheduling call;
no real initializer executes because no final module exists.

The next smallest source-derived unit is `io.c` plus `register.c`. `io.c` owns
the port table, port lifetime, and busy-port suspension invariant, and its
retained closure begins the full driver/port runtime. That is beyond this
initialization-only phase. Browser, worker, callback, import/export, and final
module size assertions remain not applicable.
