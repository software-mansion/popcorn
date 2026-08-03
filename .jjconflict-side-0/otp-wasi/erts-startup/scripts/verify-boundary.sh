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
  BIN_VH_MIN_SIZE
  erts_alcu_blockscan_init
  erts_allctrs
  erts_alloc_n_enomem
  erts_alloc_permanent_cache_aligned
  erts_check_io_interrupt
  erts_exit
  erts_init_atom_cache_map
  erts_no_dirty_alloc_instances
  erts_os_monotonic_time
  erts_port_task_init
  erts_proc_lock_init
  erts_proc_unlock_failed
  erts_sched_bif_unique_init
  erts_sspa_create
  erts_thr_fatal_error
  erts_thr_prgr__
  erts_thr_prgr_data_key__
  erts_thr_q_initialize
)

for symbol in "${expected[@]}"; do
  grep -q "undefined symbol: $symbol" "$diagnostics"
done

actual_count="$(sed -n 's/^wasm-ld: error: .*: undefined symbol: //p' "$diagnostics" | sort -u | wc -l | tr -d ' ')"
test "$actual_count" = "${#expected[@]}"
