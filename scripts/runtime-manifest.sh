#!/bin/bash
# Write the Wasm runtime manifest describing the built emulator: its OTP
# version and whether it links the crypto NIFs.
#
# Usage:
#   scripts/runtime-manifest.sh [OPTIONS]
#
# Options:
#   --beam-dir <path>       OTP build directory (default: popcorn/sources/otp)
#   --outdir <path>         Directory to write manifest.json into
#   --with-crypto <bool>    Whether the emulator links the crypto NIF
#   -h, --help              Show this help
set -euo pipefail

LOG_PREFIX="MANIFEST"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Write the Wasm runtime manifest describing the built emulator.

Options:
  --beam-dir <path>       OTP build directory (default: popcorn/sources/otp)
  --outdir <path>         Directory to write manifest.json into
  --with-crypto <bool>    Whether the emulator links the crypto NIF
  -h, --help              Show this help
EOF
    exit 0
}


write_manifest() {
    local beam_dir="$1"
    local outdir="$2"
    local with_crypto="$3"
    local manifest_path="${outdir}/manifest.json"
    local otp_version

    otp_version="$(tr -d ' \n' < "${beam_dir}/OTP_VERSION")"

    mkdir -p "${outdir}"

    printf '{"vm":{"version":"%s","capabilities":{"crypto":%s}}}\n' \
        "${otp_version}" "${with_crypto}" > "${manifest_path}"

    log "Wrote runtime manifest: ${manifest_path}"
}


main() {
    local beam_dir="${PROJECT_ROOT}/popcorn/sources/otp"
    local outdir=""
    local with_crypto="false"

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                ;;
            --beam-dir)
                beam_dir="$2"
                shift 2
                ;;
            --outdir)
                outdir="$2"
                shift 2
                ;;
            --with-crypto)
                with_crypto="$2"
                shift 2
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    if [[ "${beam_dir}" != /* ]]; then
        beam_dir="${PROJECT_ROOT}/${beam_dir}"
    fi

    if [[ -z "${outdir}" ]]; then
        error "Missing required option: --outdir"
    elif [[ "${outdir}" != /* ]]; then
        outdir="${PROJECT_ROOT}/${outdir}"
    fi

    if [[ ! -d "${beam_dir}" ]]; then
        error "OTP build directory not found at ${beam_dir}"
    fi

    case "${with_crypto}" in
        true|false) ;;
        *) error "--with-crypto expects true or false, got '${with_crypto}'" ;;
    esac

    write_manifest "${beam_dir}" "${outdir}" "${with_crypto}"
}

main "$@"
