#!/bin/bash
set -e

# Global variables for temp directory and atomvm directory
TEMP_DIR=""
ATOMVM_DIR=""

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 <output_dir>"
    echo ""
    echo "Optionally loads .env file in same dir as script, uses RUNTIME_SOURCE as git repo with branch"
    echo "or absolute path, builds AtomVM with debug-wasm, and copies artifacts to output directory."
    echo ""
    echo "Arguments:"
    echo "  output_dir    - Directory where AtomVM.wasm and AtomVM.mjs will be placed (e.g. 'assets/')"
    echo ""
    echo "Environment:"
    echo "  RUNTIME_SOURCE - Git repository URL with branch or absolute path to AtomVM source"
    echo ""
    echo "Examples:"
    echo "With git repository and branch:"
    echo "  RUNTIME_SOURCE='https://github.com/software-mansion-labs/FissionVM.git#swm' ./get_atomvm.sh assets/"
    echo "With local path:"
    echo "  RUNTIME_SOURCE='/path/to/local/atomvm' ./get_atomvm.sh assets/"
    exit 1
}

log() {
    echo -e "${YELLOW}[GET ATOMVM]${NC} $1"
}

error() {
    echo -e "${RED}[GET ATOMVM: ERROR]${NC} $1"
    exit 1
}

load_env() {
    local script_dir="$1"
    local js_dir
    js_dir="$(dirname "${script_dir}")"
    local env_file="${js_dir}/.env"

    if [[ -f "${env_file}" ]]; then
        log "Loading .env from ${env_file}"
        set -a
        # shellcheck source=/dev/null
        source "${env_file}"
        set +a
    fi
}

validate_runtime_source() {
    if [[ -z "${RUNTIME_SOURCE}" ]]; then
        error "RUNTIME_SOURCE environment variable is not set. Please set RUNTIME_SOURCE in your 'js/.env' file or in environment."
    fi
}

cleanup_old_temps() {
    local temp_base_dir
    local temp_file
    temp_file=$(mktemp -u)
    temp_base_dir=$(dirname "${temp_file}")

    # Find and remove popcorn-* directories older than 7 days
    find "${temp_base_dir}" -maxdepth 1 -name "popcorn-*" -type d -mtime +7 -exec rm -rf {} \; 2>/dev/null || true
}

setup_atomvm_source() {
    # Check if RUNTIME_SOURCE is a git URL or local path
    if [[ "${RUNTIME_SOURCE}" =~ ^https?://|^git@ ]]; then
        # It's a git repository - branch is required
        if [[ "${RUNTIME_SOURCE}" != *"#"* ]]; then
            error "Branch is required for git repository. Use format: repo_url#branch"
        fi

        local repo_url="${RUNTIME_SOURCE%#*}"
        local branch="${RUNTIME_SOURCE#*#}"

        # Create temp directory with branch name in template
        local branch_name="${branch}"
        # Sanitize branch name for filesystem
        branch_name="${branch_name//[^a-zA-Z0-9._-]/_}"
        TEMP_DIR=$(mktemp -d -t "popcorn-${branch_name}")

        log "Using git repository: ${repo_url} (branch: ${branch})"
        git clone --depth 1 --branch "${branch}" "${repo_url}" "${TEMP_DIR}/atomvm"
        ATOMVM_DIR="${TEMP_DIR}/atomvm"
    else
        # It's a local path
        if [[ ! -d "${RUNTIME_SOURCE}" ]]; then
            error "Local path '${RUNTIME_SOURCE}' does not exist"
        fi

        log "Using local path: ${RUNTIME_SOURCE}"
        ATOMVM_DIR="${RUNTIME_SOURCE}"
    fi
}

build_atomvm() {
    local atomvm_dir="$1"

    pushd "${atomvm_dir}" >/dev/null
    ./build-fission.sh debug-wasm
    popd >/dev/null
}

copy_artifacts() {
    local atomvm_dir="$1"
    local output_dir="$2"
    local out_dir="${atomvm_dir}/out"
    local wasm_file="${out_dir}/AtomVM.wasm"
    local mjs_file="${out_dir}/AtomVM.mjs"

    if [[ ! -f "${wasm_file}" ]]; then
        error "AtomVM.wasm not found at ${wasm_file}"
    fi

    if [[ ! -f "${mjs_file}" ]]; then
        error "AtomVM.mjs not found at ${mjs_file}"
    fi

    mkdir -p "${output_dir}"
    cp "${wasm_file}" "${output_dir}/"
    cp "${mjs_file}" "${output_dir}/"
}

cleanup() {
    if [[ -n "${TEMP_DIR}" ]] && [[ -d "${TEMP_DIR}" ]]; then
        rm -rf "${TEMP_DIR}"
    fi
}


main() {
    if [[ $# -ne 1 ]]; then
        error "Output directory argument is required"
    fi

    # Setup trap at the start
    trap cleanup EXIT

    local output_dir="$1"
    local script_dir
    script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

    load_env "${script_dir}"
    validate_runtime_source

    # Clean up old temp directories
    cleanup_old_temps


    setup_atomvm_source
    build_atomvm "${ATOMVM_DIR}"
    copy_artifacts "${ATOMVM_DIR}" "${output_dir}"

    log "AtomVM artifacts copied to ${output_dir}"
}

main "$@"
