#!/usr/bin/env bash
set -euo pipefail

bash scripts/compile-erl-process.sh
bash scripts/link-erts-init.sh
wasm-tools validate public/program.wasm

inspection="$(wasm-tools print public/program.wasm)"
grep -q '(import "env" "memory" (memory' <<<"$inspection"
grep -q '(import "experiment" "progress" (func' <<<"$inspection"
grep -q '(import "experiment" "report" (func' <<<"$inspection"
if grep -q 'thread-spawn' <<<"$inspection"; then
  echo 'The initialization-only module must not import wasi.thread-spawn.' >&2
  exit 1
fi
grep -q '(export "_start" (func' <<<"$inspection"
grep -q '(export "wasi_thread_start" (func' <<<"$inspection"
