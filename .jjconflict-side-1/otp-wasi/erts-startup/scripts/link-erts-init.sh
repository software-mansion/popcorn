#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${WASI_SDK_PATH:-}" ]]; then
  echo 'WASI_SDK_PATH must point at a wasi-sdk-33 installation.' >&2
  exit 1
fi

clang="$WASI_SDK_PATH/bin/clang"
otp_source="${OTP_SOURCE_PATH:-../../otp/sources/otp}"
erts="$otp_source/erts"
generated="$erts/emulator/wasm32-unknown-emscripten/opt/emu"

common_flags=(
  --target=wasm32-wasip1-threads
  --sysroot="$WASI_SDK_PATH/share/wasi-sysroot"
  -O2
  -ffunction-sections
  -fdata-sections
  -pthread
  -matomics
  -mbulk-memory
  -D_GNU_SOURCE
  -DERTS_EMULATOR
  -DUSE_THREADS
  -DHAVE_CONFIG_H
  -D_WASI_EMULATED_SIGNAL
  -D_WASI_EMULATED_PROCESS_CLOCKS
  -D_WASI_EMULATED_MMAN
  -mllvm -wasm-enable-sjlj
  -include config/wasi_compat.h
  -Iconfig
  -I"$erts/wasm32-unknown-emscripten"
  -I"$erts/emulator/wasm32-unknown-emscripten"
  -I"$generated"
  -I"$erts/emulator/beam"
  -I"$erts/emulator/sys/unix"
  -I"$erts/emulator/sys/common"
  -I"$erts/emulator/openssl/include"
  -I"$erts/emulator/zlib"
  -I"$erts/include"
  -I"$erts/include/wasm32-unknown-emscripten"
  -I"$erts/include/internal"
  -I"$erts/include/internal/wasm32-unknown-emscripten"
  -I"$erts/lib_src"
)

mkdir -p public

"$clang" "${common_flags[@]}" \
  -Wl,--gc-sections \
  -Wl,--error-limit=0 \
  -Wl,--import-memory \
  -Wl,--shared-memory \
  -Wl,--max-memory=16777216 \
  -o public/program.wasm \
  src/main.c \
  src/otp_cpu_info.c \
  src/wasi_compat.c \
  "$erts/emulator/beam/erl_alloc.c" \
  "$erts/emulator/beam/erl_alloc_util.c" \
  "$erts/emulator/beam/erl_afit_alloc.c" \
  "$erts/emulator/beam/erl_ao_firstfit_alloc.c" \
  "$erts/emulator/beam/erl_bestfit_alloc.c" \
  "$erts/emulator/beam/erl_goodfit_alloc.c" \
  "$erts/emulator/beam/erl_sched_spec_pre_alloc.c" \
  "$erts/emulator/beam/erl_process_lock.c" \
  "$erts/emulator/beam/erl_ptab.c" \
  "$erts/emulator/beam/erl_thr_progress.c" \
  "$erts/emulator/beam/erl_thr_queue.c" \
  "$erts/emulator/beam/erl_port_task.c" \
  "$erts/emulator/beam/erl_bif_unique.c" \
  "$erts/emulator/beam/erl_monitor_link.c" \
  "$erts/emulator/beam/erl_proc_sig_queue.c" \
  "$erts/emulator/beam/erl_message.c" \
  "$erts/emulator/beam/copy.c" \
  "$erts/emulator/beam/erl_bits.c" \
  "$erts/emulator/beam/erl_gc.c" \
  "$erts/emulator/beam/erl_process_dict.c" \
  "$erts/emulator/beam/erl_term_hashing.c" \
  "$erts/emulator/beam/erl_nfunc_sched.c" \
  "$erts/emulator/beam/erl_msacc.c" \
  "$erts/emulator/beam/erl_trace.c" \
  "$erts/emulator/beam/beam_ranges.c" \
  "$erts/emulator/beam/erl_bif_trace.c" \
  "$erts/emulator/beam/beam_bif_load.c" \
  "$erts/emulator/beam/code_ix.c" \
  "$erts/emulator/beam/erl_node_tables.c" \
  "$erts/emulator/beam/erl_posix_str.c" \
  "$erts/emulator/beam/erl_hl_timer.c" \
  "$erts/emulator/beam/erl_time_sup.c" \
  "$erts/emulator/beam/time.c" \
  "$erts/emulator/beam/utils.c" \
  "$erts/emulator/beam/atom.c" \
  "$erts/emulator/beam/index.c" \
  "$erts/emulator/beam/hash.c" \
  "$erts/emulator/beam/erl_unicode.c" \
  "$erts/emulator/beam/big.c" \
  "$erts/emulator/beam/erl_global_literals.c" \
  "$erts/emulator/beam/erl_nif.c" \
  "$erts/emulator/beam/external.c" \
  "$erts/emulator/sys/unix/sys_time.c" \
  "$erts/emulator/sys/unix/erl_unix_sys_ddll.c" \
  "$erts/emulator/sys/common/erl_check_io.c" \
  "$erts/emulator/sys/common/erl_poll.c" \
  "$erts/emulator/beam/erl_process.c" \
  "$erts/lib_src/common/erl_printf.c" \
  "$erts/lib_src/common/erl_printf_format.c" \
  "$erts/lib_src/common/ethr_aux.c" \
  "$erts/lib_src/common/ethr_atomics.c" \
  "$erts/lib_src/common/ethr_mutex.c" \
  "$erts/lib_src/common/ethr_cbf.c" \
  "$erts/lib_src/pthread/ethread.c" \
  "$erts/lib_src/pthread/ethr_event.c" \
  -lwasi-emulated-signal \
  -lwasi-emulated-process-clocks \
  -lwasi-emulated-mman
