#!/usr/bin/env bash
set -euo pipefail

if [[ -z "${WASI_SDK_PATH:-}" ]]; then
  echo 'WASI_SDK_PATH must point at a wasi-sdk-33 installation.' >&2
  exit 1
fi

clang="$WASI_SDK_PATH/bin/clang"
sysroot="$WASI_SDK_PATH/share/wasi-sysroot"
output="public/program.wasm"

mkdir -p public

"$clang" \
  --target=wasm32-wasip1-threads \
  --sysroot="$sysroot" \
  -O2 \
  -pthread \
  -matomics \
  -mbulk-memory \
  -Wl,--import-memory \
  -Wl,--shared-memory \
  -Wl,--max-memory=16777216 \
  -Wl,--export=wasi_thread_start \
  -Wl,--allow-undefined \
  -o "$output" \
  src/main.c

wasm-tools validate "$output"
wasm-tools print "$output" > public/program.wat
