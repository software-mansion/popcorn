#!/bin/bash
# Create OTP and Elixir stdlib tarballs for the wasm BEAM build.
#
# Usage:
#   scripts/stdlib.sh [OPTIONS]
#
# Options:
#   --beam-dir <path>   OTP build directory (default: otp/sources/otp)
#   --outdir <path>     Output directory for tarballs + manifest
#   --preset <name>     App preset: core, core-crypto, all (default: core)
#   --apps <csv>        Explicit comma-separated app list
#   -h, --help          Show this help
set -euo pipefail

LOG_PREFIX="STDLIB"
# shellcheck source=_common.sh
source "$(dirname "${BASH_SOURCE[0]}")/_common.sh"

DEFAULT_ELIXIR_TAG="v1.19.5"
DEFAULT_PRESET="core"
SOURCES_DIR="${PROJECT_ROOT}/otp/sources"
ELIXIR_CORE_APPS=(elixir)
ELIXIR_ALL_APPS=(eex elixir ex_unit iex logger mix)
CORE_APPS=(kernel stdlib compiler "${ELIXIR_CORE_APPS[@]}")
CORE_CRYPTO_EXTRA_APPS=(asn1 crypto public_key ssl inets)
CORE_CRYPTO_APPS=("${CORE_APPS[@]}" "${CORE_CRYPTO_EXTRA_APPS[@]}")
SELECTED_APPS=()
SELECTED_OTP_APPS=()
SELECTED_ELIXIR_APPS=()
GENERATED_TARBALLS=()

usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Create OTP and Elixir stdlib tarballs for the wasm BEAM build.

Options:
  --beam-dir <path>   OTP build directory (default: otp/sources/otp)
  --outdir <path>     Output directory for tarballs + manifest
  --preset <name>     App preset: core, core-crypto, all (default: ${DEFAULT_PRESET})
  --apps <csv>        Explicit comma-separated app list
  -h, --help          Show this help
EOF
    exit 0
}


sha256_hash() {
    shasum -a 256 "$1" | cut -d' ' -f1
}


is_elixir_app() {
    local app="$1"
    local elixir_app

    for elixir_app in "${ELIXIR_ALL_APPS[@]}"; do
        if [[ "${elixir_app}" == "${app}" ]]; then
            return 0
        fi
    done

    return 1
}


select_otp_app_ebin_dir() {
    local beam_dir="$1"
    local app="$2"
    local bootstrap_ebin="${beam_dir}/bootstrap/lib/${app}/ebin"
    local lib_ebin="${beam_dir}/lib/${app}/ebin"

    if [[ -f "${bootstrap_ebin}/${app}.app" ]]; then
        echo "${bootstrap_ebin}"
        return 0
    fi

    if [[ -f "${lib_ebin}/${app}.app" ]]; then
        echo "${lib_ebin}"
        return 0
    fi

    return 1
}


resolve_elixir_ebin_dir() {
    local elixir_dir="$1"
    local app="$2"
    local ebin="${elixir_dir}/lib/${app}/ebin"

    if [[ ! -d "${ebin}" ]]; then
        return 1
    fi

    echo "${ebin}"
}


list_available_otp_apps() {
    local beam_dir="$1"
    local app_dir
    local app_name

    for app_dir in "${beam_dir}/bootstrap/lib"/* "${beam_dir}/lib"/*; do
        if [[ ! -d "${app_dir}/ebin" ]]; then
            continue
        fi

        app_name=$(basename "${app_dir}")
        if [[ -f "${app_dir}/ebin/${app_name}.app" ]]; then
            printf '%s\n' "${app_name}"
        fi
    done | LC_ALL=C sort -u
}


ensure_elixir_source() {
    local elixir_tag="$1"
    local elixir_tag_dir="${elixir_tag//\//-}"
    local elixir_dir="${SOURCES_DIR}/elixir-${elixir_tag_dir}"

    if [[ -d "${elixir_dir}" ]]; then
        log "Elixir source already present at ${elixir_dir}"
        echo "${elixir_dir}"
        return
    fi

    log "Cloning Elixir ${elixir_tag}..."
    mkdir -p "${SOURCES_DIR}"
    git clone --depth 1 --branch "${elixir_tag}" \
        https://github.com/elixir-lang/elixir.git "${elixir_dir}" 1>&2
    echo "${elixir_dir}"
}


build_elixir_source() {
    local beam_dir="$1"
    local elixir_dir="$2"
    local bootstrap_tools_ebin="${beam_dir}/bootstrap/lib/tools/ebin"
    local bootstrap_make_beam="${bootstrap_tools_ebin}/make.beam"
    local tools_make_source="${beam_dir}/lib/tools/src/make.erl"

    if [[ ! -x "${beam_dir}/bootstrap/bin/erl" ]] || [[ ! -x "${beam_dir}/bootstrap/bin/erlc" ]]; then
        error "OTP bootstrap binaries not found. Build bootstrap before compiling Elixir."
    fi

    if [[ ! -f "${bootstrap_make_beam}" ]]; then
        if [[ ! -f "${tools_make_source}" ]]; then
            error "OTP tools source not found at ${tools_make_source}"
        fi

        log "Compiling bootstrap make.beam..."
        mkdir -p "${bootstrap_tools_ebin}"
        "${beam_dir}/bootstrap/bin/erlc" -o "${bootstrap_tools_ebin}" "${tools_make_source}" 1>&2
    fi

    log "Compiling Elixir source..."
    (
        cd "${elixir_dir}"
        PATH="${beam_dir}/bootstrap/bin:${PATH}" \
            ERL="${beam_dir}/bootstrap/bin/erl" \
            ERLC="${beam_dir}/bootstrap/bin/erlc" \
            make clean compile 1>&2
    )
}


resolve_selected_apps() {
    local beam_dir="$1"
    local preset="$2"
    local apps_csv="$3"
    local app

    SELECTED_APPS=()

    if [[ -n "${preset}" ]] && [[ -n "${apps_csv}" ]]; then
        error "Use either --preset or --apps, not both."
    fi

    if [[ -z "${preset}" ]] && [[ -z "${apps_csv}" ]]; then
        preset="${DEFAULT_PRESET}"
    fi

    if [[ -n "${apps_csv}" ]]; then
        local IFS=','
        read -r -a SELECTED_APPS <<< "${apps_csv}"

        for app in "${SELECTED_APPS[@]}"; do
            app="${app// /}"
            if [[ -z "${app}" ]]; then
                error "Empty app name found in --apps list."
            fi
        done
        return
    fi

    case "${preset}" in
        core)
            SELECTED_APPS=("${CORE_APPS[@]}")
            ;;
        core-crypto)
            SELECTED_APPS=("${CORE_CRYPTO_APPS[@]}")
            ;;
        all)
            while IFS= read -r app; do
                [[ -n "${app}" ]] && SELECTED_APPS+=("${app}")
            done < <(list_available_otp_apps "${beam_dir}")
            SELECTED_APPS+=("${ELIXIR_ALL_APPS[@]}")
            ;;
        *)
            error "Unknown preset '${preset}'. Use: core, core-crypto, all."
            ;;
    esac
}


split_selected_apps() {
    local app

    SELECTED_OTP_APPS=()
    SELECTED_ELIXIR_APPS=()

    for app in "${SELECTED_APPS[@]}"; do
        if is_elixir_app "${app}"; then
            SELECTED_ELIXIR_APPS+=("${app}")
        else
            SELECTED_OTP_APPS+=("${app}")
        fi
    done
}


prepare_outdir() {
    local outdir="$1"
    local path

    mkdir -p "${outdir}"

    shopt -s nullglob
    for path in "${outdir}"/*.tar.gz; do
        rm -f "${path}"
    done
    shopt -u nullglob

    rm -f "${outdir}/tarballs.json"
}


create_otp_tarballs() {
    local beam_dir="$1"
    local outdir="$2"
    local app
    local app_ebin_dir
    local src_root
    local tarball

    if [[ ! -d "${beam_dir}/bootstrap/lib" ]]; then
        error "Bootstrap libs not found. Build bootstrap before packaging tarballs."
    fi

    for app in "${SELECTED_OTP_APPS[@]}"; do
        app_ebin_dir=$(select_otp_app_ebin_dir "${beam_dir}" "${app}") || {
            error "Missing ebin dir for OTP app '${app}'."
        }

        src_root="${beam_dir}"
        if [[ "${app_ebin_dir}" == "${beam_dir}/bootstrap/"* ]]; then
            src_root="${beam_dir}/bootstrap"
        fi

        tarball="${outdir}/${app}.tar.gz"
        tar -C "${src_root}" -czf "${tarball}" "lib/${app}/ebin"
        GENERATED_TARBALLS+=("${tarball}")
    done

    if [[ ${#SELECTED_OTP_APPS[@]} -gt 0 ]]; then
        log "Processed OTP apps: ${SELECTED_OTP_APPS[*]}"
    fi
}


create_elixir_tarballs() {
    local elixir_dir="$1"
    local outdir="$2"
    local app
    local tarball

    for app in "${SELECTED_ELIXIR_APPS[@]}"; do
        resolve_elixir_ebin_dir "${elixir_dir}" "${app}" >/dev/null || {
            error "Missing Elixir app ebin dir for '${app}' in ${elixir_dir}."
        }

        tarball="${outdir}/${app}.tar.gz"
        tar -C "${elixir_dir}" -czf "${tarball}" "lib/${app}/ebin"
        GENERATED_TARBALLS+=("${tarball}")
    done

    if [[ ${#SELECTED_ELIXIR_APPS[@]} -gt 0 ]]; then
        log "Processed Elixir apps: ${SELECTED_ELIXIR_APPS[*]}"
    fi
}


write_tarball_manifest() {
    local beam_dir="$1"
    local outdir="$2"
    local manifest_path="${outdir}/tarballs.json"
    local otp_version
    local app
    local tarball
    local hash

    otp_version="$(tr -d ' \n' < "${beam_dir}/OTP_VERSION")"

    {
        printf '{'
        printf '"version":"%s"' "${otp_version}"

        for app in "${SELECTED_APPS[@]}"; do
            tarball="${outdir}/${app}.tar.gz"
            if [[ ! -f "${tarball}" ]]; then
                error "Expected tarball not found: ${tarball}"
            fi

            hash=$(sha256_hash "${tarball}")
            printf ',"%s":{"tar":"/lib/%s.tar.gz","sha256":"%s"}' "${app}" "${app}" "${hash}"
        done

        printf '}\n'
    } > "${manifest_path}"

    log "Wrote tarball manifest: ${manifest_path}"
}


emit_tarball_paths() {
    local path

    for path in "${GENERATED_TARBALLS[@]}"; do
        printf '%s\n' "${path}"
    done
}


main() {
    local beam_dir="${PROJECT_ROOT}/otp/sources/otp"
    local outdir=""
    local preset=""
    local apps_csv=""
    local elixir_tag=""
    local elixir_dir=""
    local app
    local cleaned_apps=()

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
            --preset)
                preset="$2"
                shift 2
                ;;
            --apps)
                apps_csv="$2"
                shift 2
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    load_env
    elixir_tag="${ELIXIR_TAG:-${DEFAULT_ELIXIR_TAG}}"

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

    resolve_selected_apps "${beam_dir}" "${preset}" "${apps_csv}"

    if [[ -n "${apps_csv}" ]]; then
        cleaned_apps=()
        for app in "${SELECTED_APPS[@]}"; do
            app="${app// /}"
            if [[ -z "${app}" ]]; then
                error "Empty app name found in --apps list."
            fi
            cleaned_apps+=("${app}")
        done
        SELECTED_APPS=("${cleaned_apps[@]}")
    fi

    split_selected_apps

    GENERATED_TARBALLS=()

    if [[ ${#SELECTED_ELIXIR_APPS[@]} -gt 0 ]]; then
        elixir_dir=$(ensure_elixir_source "${elixir_tag}")
        build_elixir_source "${beam_dir}" "${elixir_dir}"
    fi

    prepare_outdir "${outdir}"
    create_otp_tarballs "${beam_dir}" "${outdir}"
    if [[ ${#SELECTED_ELIXIR_APPS[@]} -gt 0 ]]; then
        create_elixir_tarballs "${elixir_dir}" "${outdir}"
    fi
    write_tarball_manifest "${beam_dir}" "${outdir}"
    emit_tarball_paths
}

main "$@"
