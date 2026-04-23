#!/bin/bash
# Build OTP/BEAM for WebAssembly (wasm32-emscripten).
#
# Usage:
#   scripts/build-beam.sh [OPTIONS] <build-mode>
#
# Build modes (positional):
#   debug       Debug build
#   release     Release build
#
# Options:
#   --with-crypto         Include static OpenSSL + crypto/asn1 NIFs
#   --otp-tag <TAG>       OTP git tag to clone (default: OTP-28.3.1)
#   --source <path>       Use local OTP source instead of cloning
#   --outdir <dir>        Output directory (default: ./out)
#   --clean               Clean before building (removes otp/sources/otp)
#   -j <N>                Parallel jobs
#   -h, --help            Show this help
set -euo pipefail

LOG_PREFIX="BUILD BEAM"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

DEFAULT_OTP_TAG="OTP-28.3.1"
SOURCES_DIR="${PROJECT_ROOT}/otp/sources"

usage() {
    cat << EOF
Usage: $0 [OPTIONS] <build-mode>

Build OTP/BEAM for WebAssembly (wasm32-emscripten).

Build modes (positional):
  debug       Debug build
  release     Release build

Options:
  --with-crypto         Include static OpenSSL + crypto/asn1 NIFs
  --otp-tag <TAG>       OTP git tag to clone (default: ${DEFAULT_OTP_TAG})
  --source <path>       Use local OTP source instead of cloning
  --outdir <dir>        Output directory (default: ./out)
  --clean               Clean before building (removes otp/sources/otp)
  -j <N>                Parallel jobs
  -h, --help            Show this help

Source selection (from higher to lower priority):
  1. --source flag if provided
  2. \$OTP_SOURCE environment variable (or from .env in project root)
  3. Default: clone OTP tag ${DEFAULT_OTP_TAG}

Examples:
  $0 debug
  $0 --with-crypto release
  $0 --source /local/otp --otp-tag OTP-28.3.1 debug
EOF
    exit 0
}

ensure_emscripten() {
    if ! command -v emconfigure &> /dev/null; then
        error "emscripten is required but not found. Please install emscripten."
    fi
}

default_jobs() {
    nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null || echo 4
}


clone_otp() {
    local tag="$1"
    local original_dir="${SOURCES_DIR}/otp-original"

    if [[ -d "${original_dir}" ]]; then
        log "OTP source already cloned at ${original_dir}"
        return
    fi

    log "Cloning OTP ${tag}..."
    mkdir -p "${SOURCES_DIR}"
    git clone --depth 1 --branch "${tag}" \
        https://github.com/erlang/otp.git "${original_dir}"
    success "OTP ${tag} cloned."
}

setup_otp_source() {
    local source="$1"
    local tag="$2"
    local original_dir="${SOURCES_DIR}/otp-original"

    if [[ -n "${source}" ]]; then
        # Resolve relative paths against PROJECT_ROOT
        local resolved="${source}"
        if [[ "${resolved}" != /* ]]; then
            resolved="${PROJECT_ROOT}/${resolved}"
        fi

        if [[ ! -d "${resolved}" ]]; then
            error "Local OTP source '${source}' does not exist (resolved to: ${resolved})"
        fi

        # Use local source as otp-original (symlink or copy)
        if [[ ! -d "${original_dir}" ]]; then
            log "Copying local OTP source to otp-original..."
            cp -a "${resolved}" "${original_dir}"
        fi
    else
        clone_otp "${tag}"
    fi
}


patch_otp() {
    log "Patching OTP sources..."
    "${PROJECT_ROOT}/scripts/patch-beam.sh"
}


run_autoconf() {
    local beam_dir="$1"
    local stamp="${beam_dir}/.stamp-autoconf"

    local current_hash
    current_hash=$(hash_inputs \
        "${beam_dir}/erts/configure.ac" \
        "${beam_dir}/make/autoconf")

    if [[ -f "${stamp}" ]] && [[ "$(cat "${stamp}")" == "${current_hash}" ]]; then
        log "Autoconf up to date, skipping."
        return
    fi

    log "Regenerating autoconf scripts..."
    (cd "${beam_dir}" && ./otp_build update_configure)

    echo "${current_hash}" > "${stamp}"
    success "Autoconf done."
}


build_bootstrap() {
    local beam_dir="$1"
    local jobs="$2"

    if [[ -f "${beam_dir}/bootstrap/bin/erlc" ]]; then
        log "Native bootstrap exists, skipping."
        return
    fi

    log "Building native bootstrap..."
    (cd "${beam_dir}" && ./configure --enable-bootstrap-only)
    (cd "${beam_dir}" && make -j"${jobs}")
    success "Bootstrap complete."
}


# Prints the OpenSSL install prefix path.
build_openssl() {
    local mode="$1"
    local jobs="$2"

    log "Building OpenSSL for WASM..."
    local openssl_args=("${mode}")
    if [[ -n "${jobs}" ]]; then
        openssl_args+=("-j" "${jobs}")
    fi

    local prefix
    prefix=$("${PROJECT_ROOT}/scripts/build-openssl.sh" "${openssl_args[@]}")
    echo "${prefix}"
}


run_configure() {
    local beam_dir="$1"
    local mode="$2"
    local with_crypto="$3"
    local openssl_prefix="$4"
    local stamp="${beam_dir}/.stamp-configure"

    local current_hash
    current_hash=$(hash_inputs \
        "${beam_dir}/erts/configure" \
        "${BASH_SOURCE[0]}")
    # Include mode and crypto config in the hash
    current_hash="${current_hash}-${mode}-${with_crypto}-${openssl_prefix}"

    if [[ -f "${stamp}" ]] && [[ "$(cat "${stamp}")" == "${current_hash}" ]]; then
        log "Configure up to date, skipping."
        return
    fi

    log "Configuring for Emscripten (${mode})..."

    # Compiler flags
    if [[ "${mode}" == "debug" ]]; then
        export CFLAGS="-O0 -g -pthread"
    else
        export CFLAGS="-Os -flto -pthread"
    fi

    # Linker flags
    export LDFLAGS="-pthread -flto"
    LDFLAGS+=" -sUSE_PTHREADS=1"
    LDFLAGS+=" -sPTHREAD_POOL_SIZE=4"
    LDFLAGS+=" -sPROXY_TO_PTHREAD=1"
    LDFLAGS+=" -sENVIRONMENT=web,worker,node"
    LDFLAGS+=" -sEXPORT_ES6=1"
    LDFLAGS+=" -sINITIAL_MEMORY=64MB"
    LDFLAGS+=" -sALLOW_MEMORY_GROWTH=1"
    LDFLAGS+=" -sEXPORTED_RUNTIME_METHODS=FS,ENV,TTY,ccall,stringToNewUTF8,lengthBytesUTF8"
    LDFLAGS+=" -sEXPORTED_FUNCTIONS=['_main','_malloc','_free']"
    LDFLAGS+=" -sFORCE_FILESYSTEM=1"
    # May need removal for Emscripten 3.x+
    LDFLAGS+=" -sEMULATE_FUNCTION_POINTER_CASTS"
    LDFLAGS+=" -sEXIT_RUNTIME=1"
    LDFLAGS+=" -sMALLOC=emmalloc"
    if [[ "${mode}" == "debug" ]]; then
        LDFLAGS+=" -sASSERTIONS=2"
    else
        LDFLAGS+=" -sASSERTIONS=0"
    fi

    # Autoconf cache variables for cross-compilation
    export ac_cv_func_pthread_create=yes
    export ac_cv_header_pthread_h=yes
    export ac_cv_sizeof_short=2
    export ac_cv_sizeof_int=4
    export ac_cv_sizeof_long=4
    export ac_cv_sizeof_long_long=8
    export ac_cv_sizeof_void_p=4
    export ac_cv_c_bigendian=no

    local build_guess
    build_guess=$("${beam_dir}/make/autoconf/config.guess")

    # Build configure arguments
    local configure_args=(
        --host=wasm32-unknown-emscripten
        --build="${build_guess}"
        --prefix=/otp
        --disable-jit
        --disable-sctp
        --disable-kernel-poll
        --disable-security-hardening-flags
        --without-common_test
        --without-diameter
        --without-edoc
        --without-eldap
        --without-eunit
        --without-ftp
        --without-jinterface
        --without-reltool
        --without-snmp
        --without-tftp
        --without-tools
        --without-termcap
        --without-wx
        --without-debugger
        --without-dialyzer
        --without-et
        --without-megaco
        --without-observer
        --without-odbc
        --without-os_mon
        --without-parsetools
        --without-ssh
        --without-mnesia
        --without-xmerl
        --without-runtime_tools
        --without-dynamic-trace
        --disable-vm-probes
        --without-threadnames
        --enable-builtin-zlib
        --disable-esock
        --disable-largefile
        --without-javac
    )

    # Crypto-specific flags
    if [[ "${with_crypto}" == "true" ]] && [[ -n "${openssl_prefix}" ]]; then
        configure_args+=(
            --with-ssl="${openssl_prefix}"
            --disable-dynamic-ssl-lib
            --enable-static-nifs=yes
        )

        (cd "${beam_dir}" && \
            erl_xcomp_sysroot="${openssl_prefix}" \
            erl_xcomp_isysroot="${openssl_prefix}" \
            LIBS="-L${openssl_prefix}/lib -lcrypto -ldl -lm" \
            emconfigure ./configure \
            "${configure_args[@]}")
    else
        configure_args+=(
            --without-crypto
            --without-asn1
            --without-ssl
        )

        (cd "${beam_dir}" && emconfigure ./configure \
            "${configure_args[@]}")
    fi

    echo "${current_hash}" > "${stamp}"
    success "Configure complete."
}


build_beam() {
    local beam_dir="$1"
    local mode="$2"
    local jobs="$3"

    log "Building BEAM for WASM (${mode}, ${jobs} jobs)..."

    # Set up JS bridge link flags if the bridge exists in the patched source
    local js_bridge_dir="${beam_dir}/erts/emulator/js_bridge"
    if [[ -d "${js_bridge_dir}" ]]; then
        export EXTRA_EMCC_LINK_FLAGS="--pre-js ${js_bridge_dir}/beam-bridge.pre.js --js-library ${js_bridge_dir}/js_bridge.js"
    fi

    # erts/lib_src target dir is created on first build only
    if [[ ! -d "${beam_dir}/erts/lib_src/wasm32-unknown-emscripten" ]]; then
        (cd "${beam_dir}" && ERL_TOP="${beam_dir}" emmake make -C erts/lib_src TARGET=wasm32-unknown-emscripten)
    fi

    (cd "${beam_dir}" && emmake make TARGET=wasm32-unknown-emscripten -j"${jobs}")

    success "BEAM build complete."
}


copy_artifacts() {
    local beam_dir="$1"
    local outdir="$2"

    mkdir -p "${outdir}" "${outdir}/bin" "${outdir}/lib"

    local wasm_bin_dir="${beam_dir}/bin/wasm32-unknown-emscripten"

    if [[ -f "${wasm_bin_dir}/beam.wasm" ]]; then
        cp "${wasm_bin_dir}/beam.wasm" "${outdir}/"
        local wasm_size
        wasm_size=$(du -h "${outdir}/beam.wasm" | cut -f1)
        log "beam.wasm: ${wasm_size}"
    fi

    if [[ -f "${wasm_bin_dir}/beam.smp" ]]; then
        cp "${wasm_bin_dir}/beam.smp" "${outdir}/"
    fi

    # Copy JS glue if present
    for ext in mjs js emu; do
        if [[ -f "${wasm_bin_dir}/beam.${ext}" ]]; then
            cp "${wasm_bin_dir}/beam.${ext}" "${outdir}/"
        fi
    done

    for boot in start.boot start_clean.boot no_dot_erlang.boot vm.boot; do
        rm -f "${outdir}/bin/${boot}"
    done

    if [[ -f "${beam_dir}/bootstrap/bin/no_dot_erlang.boot" ]]; then
        cp "${beam_dir}/bootstrap/bin/no_dot_erlang.boot" "${outdir}/bin/vm.boot"
    fi

    success "Artifacts written to: ${outdir}"
}

main() {
    load_env

    local source="${OTP_SOURCE:-}"
    local otp_tag="${DEFAULT_OTP_TAG}"
    local outdir=""
    local with_crypto=false
    local clean=false
    local jobs=""
    local mode=""

    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help)
                usage
                ;;
            --with-crypto)
                with_crypto=true
                shift
                ;;
            --otp-tag)
                otp_tag="$2"
                shift 2
                ;;
            --source)
                source="$2"
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

    # Default jobs if not set
    if [[ -z "${jobs}" ]]; then
        jobs=$(default_jobs)
    fi

    local final_outdir="${outdir:-${PROJECT_ROOT}/otp/out}"
    local beam_dir="${SOURCES_DIR}/otp"

    ensure_emscripten

    # Clean if requested
    if [[ "${clean}" == "true" ]] && [[ -d "${beam_dir}" ]]; then
        log "Cleaning otp/sources/otp..."
        rm -rf "${beam_dir}"
    fi

    setup_otp_source "${source}" "${otp_tag}"

    patch_otp

    run_autoconf "${beam_dir}"

    build_bootstrap "${beam_dir}" "${jobs}"

    local openssl_prefix=""
    if [[ "${with_crypto}" == "true" ]]; then
        openssl_prefix=$(build_openssl "${mode}" "${jobs}")
    fi

    run_configure "${beam_dir}" "${mode}" "${with_crypto}" "${openssl_prefix}"

    build_beam "${beam_dir}" "${mode}" "${jobs}"

    local stdlib_preset="core"
    if [[ "${with_crypto}" == "true" ]]; then
        stdlib_preset="core-crypto"
    fi

    "${PROJECT_ROOT}/scripts/stdlib.sh" \
        --beam-dir "${beam_dir}" \
        --outdir "${final_outdir}/lib" \
        --preset "${stdlib_preset}"

    copy_artifacts "${beam_dir}" "${final_outdir}"

    success "Build completed successfully!"
}

main "$@"
