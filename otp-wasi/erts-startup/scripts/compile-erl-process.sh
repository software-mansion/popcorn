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

mkdir -p public

"$clang" \
  --target=wasm32-wasip1-threads \
  --sysroot="$WASI_SDK_PATH/share/wasi-sysroot" \
  -pthread \
  -matomics \
  -mbulk-memory \
  -D_GNU_SOURCE \
  -DERTS_EMULATOR \
  -DUSE_THREADS \
  -DHAVE_CONFIG_H \
  -D_WASI_EMULATED_SIGNAL \
  -D_WASI_EMULATED_PROCESS_CLOCKS \
  -D_WASI_EMULATED_MMAN \
  -mllvm -wasm-enable-sjlj \
  -include config/wasi_compat.h \
  -Iconfig \
  -I"$erts/wasm32-unknown-emscripten" \
  -I"$generated" \
  -I"$erts/emulator/beam" \
  -I"$erts/emulator/sys/unix" \
  -I"$erts/emulator/sys/common" \
  -I"$erts/emulator/openssl/include" \
  -I"$erts/emulator/zlib" \
  -I"$erts/include" \
  -I"$erts/include/wasm32-unknown-emscripten" \
  -I"$erts/include/internal" \
  -I"$erts/include/internal/wasm32-unknown-emscripten" \
  -I"$erts/lib_src" \
  -O2 \
  -ffunction-sections \
  -fdata-sections \
  -c \
  -o public/erl_process.o \
  "$erts/emulator/beam/erl_process.c"

"$WASI_SDK_PATH/bin/llvm-nm" public/erl_process.o | grep -q ' T erts_init_scheduling$'
"$WASI_SDK_PATH/bin/llvm-nm" public/erl_process.o | grep -q ' T erts_start_schedulers$'
wasm-tools validate public/erl_process.o
