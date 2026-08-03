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
  "$erts/emulator/beam/erl_process.c" \
  "$erts/lib_src/common/ethr_aux.c" \
  "$erts/lib_src/common/ethr_atomics.c" \
  "$erts/lib_src/common/ethr_mutex.c" \
  "$erts/lib_src/common/ethr_cbf.c" \
  "$erts/lib_src/pthread/ethread.c" \
  "$erts/lib_src/pthread/ethr_event.c" \
  -lwasi-emulated-signal \
  -lwasi-emulated-process-clocks \
  -lwasi-emulated-mman
