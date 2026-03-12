#!/bin/bash
set -e

LOG_PREFIX="BUILD ATOMVM"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

# Must be global for trap cleanup handler
TEMP_DIR=""

DEFAULT_SOURCE="https://github.com/software-mansion-labs/FissionVM.git#swm"

usage() {
    cat << EOF
Usage: $0 [OPTIONS] <build-mode>

Build AtomVM from source.

Build modes (positional):
  debug-wasm      Build debug WASM target
  release-wasm    Build release WASM target
  debug-unix      Build debug Unix target
  release-unix    Build release Unix target

Options:
  --source <path|url#ref>   AtomVM source (default: \$ATOMVM_SOURCE env or FissionVM#swm)
  --outdir <dir>            Directory for build artifacts (default: out/ in AtomVM repository)
  --cmake-opts "KEY=VAL"    Extra cmake options (space-separated, no spaces in values)
  --clean                   Clean before building
  -j <N>                    Parallel jobs (default: auto or 1 if CI env var defined)
  -h, --help                Show this help

Source selection (from higher to lower priority):
  1. --source flag if provided
  2. \$ATOMVM_SOURCE environment variable (or from .env in project root)
  3. Default: ${DEFAULT_SOURCE}

Environment:
  Place a .env file in the project root to set ATOMVM_SOURCE for local development:
    echo "ATOMVM_SOURCE=/path/to/local/AtomVM" > .env

Source can be:
  - Local path     Use directly, no clone
  - URL#ref        Shallow clone to temp dir (ref is required)

Examples:
  $0 debug-wasm
  $0 --source /local/AtomVM --outdir ./out debug-unix
  $0 --source "https://github.com/software-mansion-labs/FissionVM.git#swm" -j 1 release-wasm
EOF
    exit 0
}

cleanup() {
    if [[ -n "${TEMP_DIR}" ]] && [[ -d "${TEMP_DIR}" ]]; then
        log "Cleaning up temp directory: ${TEMP_DIR}"
        rm -rf "${TEMP_DIR}"
    fi
}

cleanup_old_temps() {
    local temp_base_dir="${TMPDIR:-/tmp}"

    # Find and remove atomvm-build-* directories older than 7 days
    find "${temp_base_dir}" -maxdepth 1 -name "atomvm-build-*" -type d -mtime +7 -exec rm -rf {} \; 2>/dev/null || true
}

resolve_source() {
    local source="$1"

    # Priority: --source flag > $ATOMVM_SOURCE env > default
    if [[ -n "${source}" ]]; then
        log "Using source from --source flag: ${source}"
    elif [[ -n "${ATOMVM_SOURCE}" ]]; then
        source="${ATOMVM_SOURCE}"
        log "Using source from \$ATOMVM_SOURCE env: ${source}"
    else
        source="${DEFAULT_SOURCE}"
        log "Using default source: ${source}"
    fi

    echo "${source}"
}

# Prints the atomvm directory path. Sets global TEMP_DIR if cloning.
setup_atomvm_source() {
    local source="$1"

    # Check if source is a git URL or local path
    if [[ "${source}" =~ ^https?://|^git@ ]]; then
        # It's a git repository - branch is required
        if [[ "${source}" != *"#"* ]]; then
            error "Branch/ref is required for git repository. Use format: repo_url#ref"
        fi

        local repo_url="${source%#*}"
        local ref="${source#*#}"

        # Create temp directory with ref name in template
        local ref_name="${ref}"
        # Sanitize ref name for filesystem
        ref_name="${ref_name//[^a-zA-Z0-9._-]/_}"
        TEMP_DIR=$(mktemp -d "${TMPDIR:-/tmp}/atomvm-build-${ref_name}.XXXXXXX")

        log "Cloning ${repo_url} (ref: ${ref})"
        git clone --depth 1 --branch "${ref}" "${repo_url}" "${TEMP_DIR}/atomvm" >&2
        echo "${TEMP_DIR}/atomvm"
    else
        # It's a local path — resolve relative paths against PROJECT_ROOT
        local resolved="${source}"
        if [[ "${resolved}" != /* ]]; then
            resolved="${PROJECT_ROOT}/${resolved}"
        fi

        if [[ ! -d "${resolved}" ]]; then
            error "Local path '${source}' does not exist (resolved to: ${resolved})"
        fi

        log "Using local path: ${resolved}"
        echo "${resolved}"
    fi
}

ensure_ninja() {
    if ! command -v ninja &> /dev/null; then
        error "ninja is required but not found. Please install ninja-build."
    fi
}

ensure_emscripten() {
    if ! command -v emcmake &> /dev/null; then
        error "emscripten is required for WASM builds but not found. Please install emscripten."
    fi
}

build_unix() {
    local build_type="$1"
    local atomvm_dir="$2"
    local outdir="$3"
    local clean="$4"
    local cmake_opts="$5"
    local jobs="$6"
    local build_dir="${atomvm_dir}/build"

    log "Building Unix target (${build_type})"

    ensure_ninja

    if [[ "${clean}" == "true" ]] && [[ -d "${build_dir}" ]]; then
        log "Cleaning build directory"
        rm -rf "${build_dir}"
    fi

    mkdir -p "${build_dir}"

    # Build cmake options
    local cmake_args=(
        "-G" "Ninja"
        "-DCMAKE_BUILD_TYPE=${build_type}"
        "-DAVM_BUILD_RUNTIME_ONLY=1"
    )

    if [[ -n "${cmake_opts}" ]]; then
        for opt in ${cmake_opts}; do
            cmake_args+=("-D${opt}")
        done
    fi

    pushd "${build_dir}" > /dev/null

    log "Running cmake..."
    cmake "${cmake_args[@]}" ..

    log "Running ninja..."
    local ninja_args=()
    if [[ -n "${jobs}" ]]; then
        ninja_args+=("-j" "${jobs}")
    fi
    ninja "${ninja_args[@]}" AtomVM

    popd > /dev/null

    local out_dir="${outdir:-${atomvm_dir}/out}"
    mkdir -p "${out_dir}"
    cp "${build_dir}/src/AtomVM" "${out_dir}/"

    success "Unix build complete. Artifacts written to: ${out_dir}"
}

build_wasm() {
    local build_type="$1"
    local atomvm_dir="$2"
    local outdir="$3"
    local clean="$4"
    local cmake_opts="$5"
    local jobs="$6"
    local build_dir="${atomvm_dir}/src/platforms/emscripten/build"

    log "Building WASM target (${build_type})"

    ensure_ninja
    ensure_emscripten

    if [[ "${clean}" == "true" ]] && [[ -d "${build_dir}" ]]; then
        log "Cleaning build directory"
        rm -rf "${build_dir}"
    fi

    mkdir -p "${build_dir}"

    # Build cmake options
    local cmake_args=(
        "-G" "Ninja"
        "-DCMAKE_BUILD_TYPE=${build_type}"
        "-DAVM_BUILD_RUNTIME_ONLY=1"
        "-DAVM_EMSCRIPTEN_ENV=web"
    )

    if [[ -n "${cmake_opts}" ]]; then
        for opt in ${cmake_opts}; do
            cmake_args+=("-D${opt}")
        done
    fi

    pushd "${build_dir}" > /dev/null

    log "Running emcmake cmake..."
    emcmake cmake "${cmake_args[@]}" ..

    log "Running ninja..."
    local ninja_args=()
    if [[ -n "${jobs}" ]]; then
        ninja_args+=("-j" "${jobs}")
    fi
    ninja "${ninja_args[@]}" AtomVM

    popd > /dev/null

    local out_dir="${outdir:-${atomvm_dir}/out}"
    mkdir -p "${out_dir}"
    cp "${build_dir}/src/AtomVM.wasm" "${out_dir}/"
    cp "${build_dir}/src/AtomVM.mjs" "${out_dir}/"

    # Create gzipped versions if gzip is available
    if command -v gzip &> /dev/null; then
        gzip -9 -k -f "${out_dir}/AtomVM.wasm"
        gzip -9 -k -f "${out_dir}/AtomVM.mjs"
    fi

    success "WASM build complete. Artifacts written to: ${out_dir}"
}

main() {
    trap cleanup EXIT

    # Load .env from project root (before arg parsing so flags can override)
    load_env

    local source=""
    local outdir=""
    local cmake_opts=""
    local clean=false
    local jobs=""
    local build_mode=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                ;;
            --source)
                source="$2"
                shift 2
                ;;
            --outdir)
                outdir="$2"
                shift 2
                ;;
            --cmake-opts)
                cmake_opts="$2"
                shift 2
                ;;
            --clean)
                clean=true
                shift
                ;;
            -j)
                jobs="$2"
                shift 2
                ;;
            debug-wasm|release-wasm|debug-unix|release-unix)
                build_mode="$1"
                shift
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    if [[ -z "${build_mode}" ]]; then
        error "Build mode is required. Use: debug-wasm, release-wasm, debug-unix, release-unix"
    fi

    # Use single-threaded build in CI to avoid OOM (unless explicitly set)
    if [[ -n "${CI}" ]] && [[ -z "${jobs}" ]]; then
        jobs="1"
        log "CI detected, using -j 1 to avoid OOM"
    fi

    cleanup_old_temps

    source=$(resolve_source "${source}")
    local atomvm_dir
    atomvm_dir=$(setup_atomvm_source "${source}")

    local build_type
    case "${build_mode}" in
        debug-*)  build_type="Debug" ;;
        release-*) build_type="Release" ;;
    esac

    case "${build_mode}" in
        *-wasm) build_wasm "${build_type}" "${atomvm_dir}" "${outdir}" "${clean}" "${cmake_opts}" "${jobs}" ;;
        *-unix) build_unix "${build_type}" "${atomvm_dir}" "${outdir}" "${clean}" "${cmake_opts}" "${jobs}" ;;
    esac

    success "Build completed successfully!"
}

main "$@"
