#!/bin/bash
set -e

LOG_PREFIX="CLEAN"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Clean build artifacts.

Options:
  --all               Clean everything (Elixir, JS, BEAM runtime, examples)
  --runtime           Clean BEAM runtime artifacts only
  --example <name>    Clean a specific example
  -h, --help          Show this help

Default (no flags): cleans Popcorn Elixir dependencies and JS output

Available examples:
$(list_examples)
EOF
    exit 0
}

clean_dir() {
    if [[ -d "$1" || -L "$1" ]]; then
        log "Removing $1"
        rm -rf "$1"
    fi
}

clean_elixir() {
    log "Cleaning Elixir build artifacts..."
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/_build"
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/deps"
}

clean_runtime() {
    log "Cleaning BEAM runtime artifacts..."
    clean_dir "${PROJECT_ROOT}/popcorn/out"
}

clean_js() {
    log "Cleaning JS build artifacts..."
    clean_dir "${PROJECT_ROOT}/popcorn/out/js"
    clean_dir "${PROJECT_ROOT}/popcorn/js/dist"
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/priv/static"
    clean_dir "${PROJECT_ROOT}/popcorn/js/node_modules"
    clean_dir "${PROJECT_ROOT}/node_modules"
}

clean_example() {
    local name="$1"
    local dir="${PROJECT_ROOT}/examples/${name}"

    if [[ ! -d "${dir}" ]]; then
        error "Example '${name}' not found at ${dir}"
    fi

    log "Cleaning example: ${name}"
    clean_dir "${dir}/_build"
    clean_dir "${dir}/deps"
    clean_dir "${dir}/dist"

    if [[ -d "${dir}/assets/node_modules" ]]; then
        clean_dir "${dir}/assets/node_modules"
    fi
}

clean_all_examples() {
    log "Cleaning all examples..."
    for dir in "${PROJECT_ROOT}"/examples/*/; do
        if [[ -f "${dir}/mix.exs" ]]; then
            clean_dir "${dir}_build"
            clean_dir "${dir}deps"
        fi
    done

    log "Cleaning all projects..."
    for dir in "${PROJECT_ROOT}"/landing-page/ "${PROJECT_ROOT}"/language-tour/ "${PROJECT_ROOT}"/local-live-view/; do
        if [[ -f "${dir}/mix.exs" ]]; then
            clean_dir "${dir}_build"
            clean_dir "${dir}deps"
        fi
    done
}

main() {
    local MODE="default"
    local EXAMPLE_NAME=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help) usage ;;
            --all) MODE="all"; shift ;;
            --runtime) MODE="runtime"; shift ;;
            --example)
                MODE="example"
                EXAMPLE_NAME="$2"
                shift 2
                ;;
            *) error "Unknown option: $1" ;;
        esac
    done

    case "${MODE}" in
        default)
            clean_elixir
            clean_js
            ;;
        all)
            clean_elixir
            clean_js
            clean_runtime
            clean_all_examples
            ;;
        runtime)
            clean_runtime
            ;;
        example)
            clean_example "${EXAMPLE_NAME}"
            ;;
    esac

    success "Clean complete!"
}

main "$@"
