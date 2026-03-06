#!/bin/bash
set -e

LOG_PREFIX="CLEAN"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

MODE="default"
EXAMPLE_NAME=""

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Clean build artifacts.

Options:
  --all               Clean everything (elixir, JS, AtomVM, examples)
  --atomvm            Clean AtomVM artifacts only
  --example <name>    Clean a specific example
  -h, --help          Show this help

Default (no flags): cleans elixir/_build, elixir/deps, js/dist

Available examples:
$(list_examples)
EOF
    exit 0
}

while [[ $# -gt 0 ]]; do
    case "$1" in
        -h|--help) usage ;;
        --all) MODE="all"; shift ;;
        --atomvm) MODE="atomvm"; shift ;;
        --example)
            MODE="example"
            EXAMPLE_NAME="$2"
            shift 2
            ;;
        *) error "Unknown option: $1" ;;
    esac
done

clean_dir() {
    if [[ -d "$1" ]]; then
        log "Removing $1"
        rm -rf "$1"
    fi
}

clean_elixir() {
    log "Cleaning Elixir build artifacts..."
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/_build"
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/deps"
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/popcorn_runtime_source/artifacts"
}

clean_atomvm() {
    log "Cleaning AtomVM artifacts..."
    clean_dir "${PROJECT_ROOT}/popcorn/elixir/popcorn_runtime_source/artifacts"
    clean_dir "${PROJECT_ROOT}/popcorn/js/assets"
}

clean_js() {
    log "Cleaning JS build artifacts..."
    clean_dir "${PROJECT_ROOT}/popcorn/js/dist"
    clean_dir "${PROJECT_ROOT}/popcorn/js/assets"
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

case "${MODE}" in
    default)
        clean_elixir
        clean_js
        ;;
    all)
        clean_elixir
        clean_js
        clean_atomvm
        clean_all_examples
        ;;
    atomvm)
        clean_atomvm
        ;;
    example)
        clean_example "${EXAMPLE_NAME}"
        ;;
esac

success "Clean complete!"
