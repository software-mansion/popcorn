#!/bin/bash
set -e

LOG_PREFIX="TEST"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

# Defaults
MODE="unix"
TEST_PATH=""

usage() {
    cat << EOF
Usage: $0 [OPTIONS] [path]

Run tests.

Options:
  --unix    Run Elixir tests with unix target (default)
  --wasm    Run Elixir tests with wasm target
  --js      Run JS tests
  -h        Show this help

Examples:
  $0                          # Run unix Elixir tests
  $0 --wasm                   # Run wasm Elixir tests
  $0 --js                     # Run JS tests
  $0 test/some_test.exs       # Run specific test file
  $0 --wasm test/some_test.exs
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help) usage ;;
        --unix) MODE="unix"; shift ;;
        --wasm) MODE="wasm"; shift ;;
        --js) MODE="js"; shift ;;
        *)
            if [[ -z "${TEST_PATH}" ]]; then
                TEST_PATH="$1"
            else
                error "Unknown argument: $1"
            fi
            shift
            ;;
    esac
done

ELIXIR_DIR="${PROJECT_ROOT}/popcorn/elixir"
ARTIFACTS_DIR="${ELIXIR_DIR}/popcorn_runtime_source/artifacts"
BUILD_SCRIPT="${SCRIPT_DIR}/build-atomvm.sh"

ensure_atomvm() {
    local target="$1"
    local artifact_dir="${ARTIFACTS_DIR}/${target}"

    case "${target}" in
        unix) local expected="${artifact_dir}/AtomVM" ;;
        wasm) local expected="${artifact_dir}/AtomVM.wasm" ;;
    esac

    if [[ ! -f "${expected}" ]]; then
        log "AtomVM artifacts not found for ${target}, building..."
        load_env
        "${BUILD_SCRIPT}" --outdir "${artifact_dir}" "debug-${target}"
    fi
}

case "${MODE}" in
    unix)
        ensure_atomvm unix
        log "Running Elixir tests (unix target)"
        cd "${ELIXIR_DIR}"
        mix deps.get
        mix test ${TEST_PATH}
        ;;
    wasm)
        ensure_atomvm wasm
        log "Running Elixir tests (wasm target)"
        cd "${ELIXIR_DIR}"
        mix deps.get
        TARGET=wasm mix test ${TEST_PATH}
        ;;
    js)
        log "Running JS e2e tests"
        cd "${PROJECT_ROOT}/popcorn/js"
        pnpm install
        pnpm test:e2e ${TEST_PATH}
        ;;
esac

success "Tests passed!"
