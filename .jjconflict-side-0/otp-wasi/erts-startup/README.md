# WASI/ERTS scheduling initialization

This proof of concept compiles and executes OTP 28.3.1's unchanged
`erts_init_scheduling()` for `wasm32-wasip1-threads`. It calls exactly:

```c
erts_init_scheduling(1, 1, 1, 1, 1, 1);
```

The function returns normally on the VM/main browser worker. The POC does not
call `erts_start_schedulers()`, create child workers, or import
`wasi.thread-spawn`.

## Result

**Go.** The source-derived initialization chain executes through
`erts_init_process()`, enters the real scheduling initializer, creates the run
queues and scheduler state, and returns normally.

The structured browser result is:

- normal schedulers: 1;
- dirty CPU schedulers: 1;
- dirty I/O schedulers: 1;
- poll threads: 1;
- run queues: 1 normal plus OTP's two dirty run queues;
- total browser workers: 1 VM/main worker;
- peak active workers: 1;
- active workers after teardown: 0;
- `wasi.thread-spawn` calls: 0.

## Source boundary

The link command in `scripts/link-erts-init.sh` is the exact source manifest.
All OTP 28.3.1 sources are unchanged. In addition to ethread and the earlier
allocator, process-table, thread-progress, polling, time, monitor, and
process-signal prerequisites, the final closure includes:

- messages and off-heap cleanup: `erl_message.c`;
- copying and bits: `copy.c`, `erl_bits.c`;
- garbage collection: `erl_gc.c`;
- process dictionaries and hashing: `erl_process_dict.c`,
  `erl_term_hashing.c`;
- NIF scheduling and microstate accounting: `erl_nfunc_sched.c`, `erl_msacc.c`;
- tracing and sessions: `erl_trace.c`, `beam_ranges.c`, `erl_bif_trace.c`;
- literal release and code indexes: `beam_bif_load.c`, `code_ix.c`;
- nodes and distribution entries: `erl_node_tables.c`;
- port tables, lifetime, and suspension: `io.c`;
- registered-name lookup: `register.c`;
- driver-handle lifetime: `erl_bif_ddll.c`;
- map construction: `erl_map.c`;
- the Unix dynamic-loader platform layer: `erl_unix_sys_ddll.c`.

No wasi-libc source or pthread implementation is patched. The only local OTP
feature override added by this phase is `HAVE_DLOPEN`: WASI has no dynamic
loader, so unchanged `erl_unix_sys_ddll.c` selects OTP's existing
`ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY` branch.

## WASI compatibility seams

`src/wasi_compat.c` contains only platform behavior:

- ERTS system and aligned allocation through wasi-libc allocation;
- fatal exit and assertion termination;
- fixed WASI OS version/flavor reporting;
- unsupported pthread process contention scope;
- a fixed CPU-count query in `src/otp_cpu_info.c`;
- a wakeup descriptor pair for `erts_init_check_io()`.

The wakeup pair uses descriptors 3 and 4, backed by the browser host's WASI FD
imports. ERTS creates and configures this pair while initializing its pollset.
No polling or scheduler thread starts in this phase, so no blocking wakeup I/O
occurs. This implements the missing WASI `pipe()` OS seam; it does not replace
ERTS polling, port, or scheduler behavior.

Unsupported APIs are dynamic loading, account lookup, resolver metadata,
external signals, and real pipe-backed blocking I/O. None is needed after the
bounded initialization returns.

## Initialization order

The harness follows the relevant ordering from `erl_init.c`:

1. ethread and system time support;
2. thread-progress pre-initialization and early scheduling state;
3. allocators;
4. check-I/O and pollset initialization;
5. thread progress, thread queues, and late ethread allocation callbacks;
6. monitor/link, scheduler-unique, and process-signal infrastructure;
7. time initialization;
8. process table initialization;
9. `erts_init_scheduling(1, 1, 1, 1, 1, 1)`.

Structured callbacks report every required milestone, including entry and
normal return from `erts_init_scheduling()`.

## Commands

From this directory:

```sh
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos pnpm build
pnpm lint
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos \
  bash scripts/verify-boundary.sh
WASI_SDK_PATH=/path/to/wasi-sdk-33.0-arm64-macos \
  pnpm exec playwright test --repeat-each=5 --workers=1
```

## Verification

- shell syntax: passed;
- TypeScript checking: passed;
- wasi-sdk 33 `wasm32-wasip1-threads` compilation and final link: passed;
- `wasm-tools validate`: passed;
- exact Wasm import/export assertions: passed;
- focused Chromium test: passed;
- five serial Chromium repeats: 5/5 passed;
- child workers and `wasi.thread-spawn` calls: zero;
- active workers after teardown: zero.

The final module is 680,721 bytes raw, 196,028 bytes with Brotli-11, and
234,848 bytes with gzip-9. Exact imports and exports are recorded in
`module-inspection.txt`. Generated Wasm, bundles, reports, toolchains, and
browser artifacts remain ignored.
