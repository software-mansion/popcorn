#!/bin/bash
# Common utilities for all scripts.
# Usage: set LOG_PREFIX before sourcing this file.
#   LOG_PREFIX="TEST"
#   source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[1]}")" && pwd)"
PROJECT_ROOT="$(dirname "${SCRIPT_DIR}")"

# Color codes
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

: "${LOG_PREFIX:=SCRIPT}"

log() { echo -e "${BLUE}${LOG_PREFIX} |${NC} $1" >&2; }
success() { echo -e "${GREEN}${LOG_PREFIX} |${NC} $1" >&2; }
error() { echo -e "${RED}${LOG_PREFIX}: ERROR |${NC} $1" >&2; exit 1; }

list_examples() {
    for dir in "${PROJECT_ROOT}/examples"/*/; do
        echo "  $(basename "${dir}")"
    done
}

list_projects() {
    for name in landing-page language-tour local-live-view; do
        if [[ -d "${PROJECT_ROOT}/${name}" ]]; then
            echo "  ${name}"
        fi
    done
}

load_env() {
    local env_file="${PROJECT_ROOT}/.env"
    if [[ -f "${env_file}" ]]; then
        log "Loading .env from ${env_file}"
        set -a
        # shellcheck source=/dev/null
        source "${env_file}"
        set +a
    fi
}

install_elixir_deps() {
    local elixir_dir="$1"
    local label="${2:-Elixir deps}"

    log "Installing ${label}..."
    (cd "${elixir_dir}" && mix deps.get)
}

install_pnpm_deps() {
    local dir="$1"
    local label="${2:-pnpm deps}"

    log "Installing ${label}..."
    (cd "${dir}" && pnpm install)
}

install_pnpm_workspace_deps() {
    local repo_root="$1"
    local label="${2:-pnpm workspace deps}"

    log "Installing ${label}..."
    (cd "${repo_root}" && pnpm install --frozen-lockfile)
}
