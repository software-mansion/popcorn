#!/bin/bash
set -e

LOG_PREFIX="TEST"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

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

main() {
    local MODE="unix"
    local TEST_PATH=""

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

    local elixir_dir="${PROJECT_ROOT}/popcorn/elixir"
    case "${MODE}" in
        unix)
            log "Running Elixir tests (unix target)"
            install_elixir_deps "${elixir_dir}" "Elixir deps"
            cd "${elixir_dir}"
            mix test ${TEST_PATH}
            ;;
        wasm)
            log "Running Elixir tests (wasm target)"
            install_elixir_deps "${elixir_dir}" "Elixir deps"
            install_pnpm_workspace_deps "${PROJECT_ROOT}" "pnpm workspace deps"
            cd "${elixir_dir}"
            TARGET=wasm mix test ${TEST_PATH}
            ;;
        js)
            log "Running JS e2e tests"
            install_pnpm_workspace_deps "${PROJECT_ROOT}" "pnpm workspace deps"
            cd "${PROJECT_ROOT}/popcorn/js"
            pnpm test:e2e ${TEST_PATH}
            ;;
    esac

    success "Tests passed!"
}

main "$@"
