#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${WASI_SDK_PATH:-}" ]]; then
  echo 'WASI_SDK_PATH must point at a wasi-sdk-33 installation.' >&2
  exit 1
fi

clang="$WASI_SDK_PATH/bin/clang"
otp_source="${OTP_SOURCE_PATH:-../../otp/sources/otp}"
internal="$otp_source/erts/include/internal"
lib_src="$otp_source/erts/lib_src"
output="public/program.wasm"

test -f "$lib_src/pthread/ethread.c"
mkdir -p public

"$clang" \
  --target=wasm32-wasip1-threads \
  --sysroot="$WASI_SDK_PATH/share/wasi-sysroot" \
  -O2 \
  -flto \
  -ffunction-sections \
  -fdata-sections \
  -pthread \
  -matomics \
  -mbulk-memory \
  -DHAVE_CONFIG_H \
  -D_THREAD_SAFE \
  -D_REENTRANT \
  -DPOSIX_THREADS \
  -D_WASI_EMULATED_SIGNAL \
  -include config/wasi_compat.h \
  -Iconfig \
  -I"$internal" \
  -Wl,--gc-sections \
  -Wl,--import-memory \
  -Wl,--shared-memory \
  -Wl,--max-memory=16777216 \
  -Wl,--export=wasi_thread_start \
  -Wl,--allow-undefined \
  -o "$output" \
  src/main.c \
  src/otp_cpu_info.c \
  src/wasi_compat.c \
  "$lib_src/common/ethr_aux.c" \
  "$lib_src/common/ethr_atomics.c" \
  "$lib_src/common/ethr_mutex.c" \
  "$lib_src/common/ethr_cbf.c" \
  "$lib_src/pthread/ethread.c" \
  "$lib_src/pthread/ethr_event.c" \
  -lwasi-emulated-signal

wasm-tools validate "$output"
wasm-tools print "$output" > public/program.wat
