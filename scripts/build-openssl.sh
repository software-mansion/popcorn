#!/bin/bash
set -euo pipefail

LOG_PREFIX="BUILD OPENSSL"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

DEFAULT_TAG="3.0.15"
SOURCES_DIR="${PROJECT_ROOT}/otp/sources"

usage() {
    cat << EOF
Usage: $0 [OPTIONS] <build-mode>

Build OpenSSL for WebAssembly (wasm32-emscripten).

Build modes (positional):
  debug       Debug build (-O0 -g)
  release     Release build (-Os)

Options:
  --tag <TAG>       OpenSSL version tag (default: ${DEFAULT_TAG})
  --outdir <dir>    Output directory for installed artifacts (default: <source-dir>-installed)
  --clean           Clean before building
  -j <N>            Parallel jobs (default: auto or 1 if CI env var defined)
  -h, --help        Show this help

Source is cloned once per (tag, mode) pair into:
  otp/sources/openssl-<tag>-<mode>/

If the clone and build artifacts already exist, artifacts are copied to outdir
without rebuilding. Use --clean to force a rebuild.

Examples:
  $0 debug
  $0 --tag 3.0.15 release
  $0 --outdir ./out/openssl release
EOF
    exit 0
}

ensure_emscripten() {
    if ! command -v emcc &> /dev/null; then
        error "emscripten is required but not found. Please install emscripten."
    fi
}

# Returns the source directory path for a given tag and mode.
source_dir() {
    local tag="$1" mode="$2"
    echo "${SOURCES_DIR}/openssl-${tag}-${mode}"
}

# Returns the install prefix directory for a given tag and mode.
install_dir() {
    local tag="$1" mode="$2"
    echo "${SOURCES_DIR}/openssl-${tag}-${mode}-installed"
}

# Clone OpenSSL source if the directory doesn't exist yet.
clone_source() {
    local tag="$1" src_dir="$2"

    if [[ -d "${src_dir}" ]]; then
        log "Source directory already exists: ${src_dir}"
        return
    fi

    log "Cloning OpenSSL ${tag}..."
    mkdir -p "${SOURCES_DIR}"
    git clone --depth 1 --branch "openssl-${tag}" \
        "https://github.com/openssl/openssl.git" "${src_dir}"
}

# Check if build artifacts are already present in the install prefix.
has_artifacts() {
    local prefix="$1"
    [[ -f "${prefix}/include/openssl/opensslv.h" ]] \
        && [[ -f "${prefix}/lib/libcrypto.a" ]]
}

configure_openssl() {
    local mode="$1" src_dir="$2" prefix="$3"

    local cflags
    if [[ "${mode}" == "debug" ]]; then
        cflags="-O0 -g -pthread"
    else
        cflags="-Os -pthread"
    fi

    log "Cleaning previous build state..."
    (cd "${src_dir}" && make distclean &> /dev/null || true)

    log "Running Configure (${mode})..."
    (cd "${src_dir}" && \
        CC=emcc \
        AR=emar \
        RANLIB=emranlib \
        CFLAGS="${cflags}" \
        ./Configure \
            linux-generic32 \
            no-shared \
            no-dso \
            no-module \
            no-asm \
            no-afalgeng \
            no-devcryptoeng \
            no-tests \
            no-cms \
            no-ct \
            no-ocsp \
            no-ts \
            no-sock \
            no-dgram \
            no-srtp \
            no-comp \
            no-cast \
            no-idea \
            no-seed \
            no-camellia \
            no-whirlpool \
            no-siphash \
            no-ec2m \
            no-ui-console \
            no-sm2 \
            no-sm3 \
            no-sm4 \
            no-blake2 \
            no-bf \
            no-rc2 \
            no-rc4 \
            no-rmd160 \
            no-md4 \
            no-engine \
            no-srp \
            no-dsa \
            no-legacy \
            --prefix="${prefix}" \
            --openssldir="${prefix}/ssl") >&2
}

build_libs() {
    local src_dir="$1" jobs="$2"

    log "Building libs..."
    local make_args=("-j" "${jobs:-$(nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4)}")
    (cd "${src_dir}" && emmake make build_libs "${make_args[@]}") >&2
}

install_dev() {
    local src_dir="$1"

    log "Installing dev artifacts..."
    (cd "${src_dir}" && emmake make install_dev) >&2
}

copy_artifacts() {
    local prefix="$1" outdir="$2"

    if [[ "${prefix}" == "${outdir}" ]]; then
        return
    fi

    log "Copying artifacts to ${outdir}..."
    mkdir -p "${outdir}"
    cp -R "${prefix}/include" "${outdir}/"
    cp -R "${prefix}/lib" "${outdir}/"
    if [[ -d "${prefix}/ssl" ]]; then
        cp -R "${prefix}/ssl" "${outdir}/"
    fi
}

main() {
    load_env

    local tag="${DEFAULT_TAG}"
    local outdir=""
    local clean=false
    local jobs=""
    local mode=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                ;;
            --tag)
                tag="$2"
                shift 2
                ;;
            --outdir)
                outdir="$2"
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
            debug|release)
                mode="$1"
                shift
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    if [[ -z "${mode}" ]]; then
        error "Build mode is required. Use: debug, release"
    fi

    # Use single-threaded build in CI to avoid OOM (unless explicitly set)
    if [[ -n "${CI:-}" ]] && [[ -z "${jobs}" ]]; then
        jobs="1"
        log "CI detected, using -j 1 to avoid OOM"
    fi

    ensure_emscripten

    local src_dir
    src_dir=$(source_dir "${tag}" "${mode}")
    local prefix
    prefix=$(install_dir "${tag}" "${mode}")
    local target_outdir="${outdir:-${prefix}}"

    # If artifacts exist and no --clean, just copy to outdir
    if [[ "${clean}" == "false" ]] && [[ -d "${src_dir}" ]] && has_artifacts "${prefix}"; then
        log "OpenSSL ${tag} (${mode}) already built, skipping build."
        copy_artifacts "${prefix}" "${target_outdir}"
        # Print prefix path for callers
        echo "${target_outdir}"
        success "OpenSSL artifacts ready at: ${target_outdir}"
        return
    fi

    if [[ "${clean}" == "true" ]] && [[ -d "${src_dir}" ]]; then
        log "Cleaning source directory"
        rm -rf "${src_dir}"
        rm -rf "${prefix}"
    fi

    clone_source "${tag}" "${src_dir}"
    configure_openssl "${mode}" "${src_dir}" "${prefix}"
    build_libs "${src_dir}" "${jobs}"
    install_dev "${src_dir}"

    copy_artifacts "${prefix}" "${target_outdir}"
    # Print prefix path for callers
    echo "${target_outdir}"
    success "OpenSSL ${tag} (${mode}) build complete. Artifacts at: ${target_outdir}"
}

main "$@"
