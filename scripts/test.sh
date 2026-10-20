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
  (no flag)           Run Popcorn Elixir tests
  --js                Run popcorn JS e2e tests
  --popdoc            Run popdoc e2e tests
  -h                  Show this help

Examples:
  $0                          # Run Popcorn Elixir tests
  $0 --js                     # Run popcorn JS e2e tests
  $0 --popdoc                 # Run popdoc e2e tests
  $0 test/some_test.exs       # Run specific test file
EOF
    exit 0
}

main() {
    local MODE="elixir"
    local TEST_PATH=()

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help) usage ;;
            --js) MODE="js"; shift ;;
            --popdoc) MODE="popdoc"; shift ;;
            --*) error "Unknown option: $1" ;;
            *)
                if [[ ${#TEST_PATH[@]} -eq 0 ]]; then
                    TEST_PATH=("$1")
                else
                    error "Unknown argument: $1"
                fi
                shift
                ;;
        esac
    done

    local elixir_dir="${PROJECT_ROOT}/popcorn/elixir"
    case "${MODE}" in
        elixir)
            log "Running Popcorn Elixir tests"
            install_elixir_deps "${elixir_dir}" "Elixir deps"
            cd "${elixir_dir}"
            mix test "${TEST_PATH[@]}"
            ;;
        js)
            log "Running JS e2e tests"
            install_pnpm_workspace_deps "${PROJECT_ROOT}" "pnpm workspace deps"
            cd "${PROJECT_ROOT}/popcorn/js"
            pnpm e2e "${TEST_PATH[@]}"
            ;;
        popdoc)
            log "Running popdoc e2e tests"
            install_elixir_deps "${PROJECT_ROOT}/popdoc/e2e/fixture" "popdoc fixture deps"
            install_pnpm_workspace_deps "${PROJECT_ROOT}" "pnpm workspace deps"
            cd "${PROJECT_ROOT}/popdoc/e2e"
            pnpm test:e2e "${TEST_PATH[@]}"
            ;;
    esac

    success "Tests passed!"
}

main "$@"
