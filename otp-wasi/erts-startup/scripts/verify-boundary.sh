#!/usr/bin/env bash
set -euo pipefail

diagnostics="$(mktemp)"
trap 'rm -f "$diagnostics"' EXIT

bash scripts/compile-erl-process.sh

if bash scripts/link-erts-init.sh >"$diagnostics" 2>&1; then
  echo 'Expected the bounded ERTS initialization link to fail.' >&2
  exit 1
fi

expected=(
  erts_port
  erts_port_free
  erts_save_suspend_process_on_port
  erts_whereis_name_to_id
)

for symbol in "${expected[@]}"; do
  grep -q "undefined symbol: $symbol" "$diagnostics"
done

actual_count="$(sed -n 's/^wasm-ld: error: .*: undefined symbol: //p' "$diagnostics" | sort -u | wc -l | tr -d ' ')"
test "$actual_count" = "${#expected[@]}"
