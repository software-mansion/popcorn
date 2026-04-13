#!/bin/bash
# Patch OTP/BEAM sources for WebAssembly (wasm32-emscripten).
#
# Usage:
#   scripts/patch-beam.sh [OPTIONS]
#
# Modes:
#   (default)     Apply patches from otp/patches/ to otp/sources/otp
#   --regen       Regenerate patches from current otp/sources/otp state
#
# Options:
#   -h, --help    Show this help
#
# Requires otp/sources/otp to exist (created by build-beam.sh).
# Patches are stored in otp/patches/.
set -euo pipefail

LOG_PREFIX="PATCH BEAM"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

SOURCES_DIR="${PROJECT_ROOT}/otp/sources"
OTP_DIR="${SOURCES_DIR}/otp"
OTP_ORIGINAL_DIR="${SOURCES_DIR}/otp-original"
PATCHES_DIR="${PROJECT_ROOT}/otp/patches"
STAMP_FILE="${OTP_DIR}/.stamp-patched"

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Patch OTP/BEAM sources for WebAssembly (wasm32-emscripten).

Modes:
  (default)     Apply patches from otp/patches/ to otp/sources/otp
  --regen       Regenerate patches from current otp/sources/otp state

Options:
  -h, --help    Show this help

Apply mode:
  Expects otp/sources/otp-original to exist (cloned OTP).
  Copies otp-original to otp/ if not present, then applies patches.

Regen mode:
  Expects otp/sources/otp to exist with local modifications.
  Stages changes, excludes generated files, writes patches to otp/patches/.
EOF
    exit 0
}

# Add build artifacts and generated files to .git/info/exclude.
# Idempotent — skips if already done.
setup_git_excludes() {
    local exclude_file="${OTP_DIR}/.git/info/exclude"
    local marker="# otp-wasm excludes"

    if grep -q "${marker}" "${exclude_file}" 2>/dev/null; then
        log "Git excludes already configured."
        return
    fi

    log "Adding build artifacts to .git/info/exclude..."
    cat >> "${exclude_file}" << 'EXCLUDE'
# otp-wasm excludes
*.o
*.a
*.so
*.dylib
*.beam
*.wasm
/bootstrap/
/bin/
/stdlib-tarballs/
/.openssl-wasm/
**/wasm32-unknown-emscripten/
**/config.status
**/config.log
.stamp-*
**/configure
**/config.h.in
/autom4te.cache/
/lib/*/autom4te.cache/
/cross_check_erl.erl
EXCLUDE
}

apply_patches() {
    if [[ ! -d "${OTP_ORIGINAL_DIR}" ]]; then
        error "otp/sources/otp-original not found. Run build-beam.sh first to clone OTP."
    fi

    if [[ ! -d "${PATCHES_DIR}" ]]; then
        error "otp/patches/ not found. No patches to apply."
    fi

    local patches
    patches=$(find "${PATCHES_DIR}" -name "*.patch" -type f | sort)
    if [[ -z "${patches}" ]]; then
        error "No .patch files found in otp/patches/."
    fi

    # Copy otp-original to otp if not present
    if [[ ! -d "${OTP_DIR}" ]]; then
        log "Copying otp-original to otp..."
        cp -a "${OTP_ORIGINAL_DIR}" "${OTP_DIR}"
    fi

    # Check if already patched
    if [[ -f "${STAMP_FILE}" ]]; then
        log "Patches already applied, skipping."
        return
    fi

    setup_git_excludes

    log "Applying patches..."
    while IFS= read -r patch; do
        local patch_name
        patch_name=$(basename "${patch}")
        log "  Applying ${patch_name}..."
        (cd "${OTP_DIR}" && git apply "${patch}")
    done <<< "${patches}"

    touch "${STAMP_FILE}"
    success "Patches applied."
}

regen_patches() {
    if [[ ! -d "${OTP_DIR}" ]]; then
        error "otp/sources/otp not found. Nothing to regenerate from."
    fi

    if [[ ! -d "${OTP_DIR}/.git" ]]; then
        error "otp/sources/otp is not a git repository."
    fi

    mkdir -p "${PATCHES_DIR}"

    # Find the base revision (the initial commit from the shallow clone)
    local base_rev
    base_rev=$(git -C "${OTP_DIR}" rev-list --max-parents=0 HEAD)

    log "Base revision: ${base_rev}"

    # Stage all source changes (build artifacts excluded via .git/info/exclude)
    git -C "${OTP_DIR}" add -A

    # Unstage generated files that must not land in the source patch
    git -C "${OTP_DIR}" diff --cached --name-only "${base_rev}" \
        | grep -E '(^|/)\.openssl-wasm/|(^|/)autom4te\.cache/|(/configure|config\.h\.in|config\.log|config\.status|cross_check_erl\.erl)$' \
        | xargs -I{} git -C "${OTP_DIR}" reset "${base_rev}" -- {} > /dev/null 2>&1 || true

    # Write patch
    local patch_file="${PATCHES_DIR}/0001-emscripten-support.patch"
    git -C "${OTP_DIR}" diff --cached --full-index "${base_rev}" \
        > "${patch_file}"

    local lines
    lines=$(wc -l < "${patch_file}" | tr -d ' ')
    success "Patch regenerated: ${patch_file} (${lines} lines)"
}

main() {
    local regen=false

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                ;;
            --regen)
                regen=true
                shift
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    if [[ "${regen}" == "true" ]]; then
        regen_patches
    else
        apply_patches
    fi
}

main "$@"
